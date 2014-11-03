' TODO LIST:
' * Optimize the routine to deal with larger files more effectively.
' * Look into processing on more than one thread.

' NOTES:
' * You can pretty much blame any indenting issues on Visual Studio 2013.

' Preprocessor related:

' WARNING: This functionality has not been tested recently, and may not be in working order.
' This does what the name suggests, it uses a shared cache for stream-caching.
' Obviously, this also means that this will make multiple threads impossible (Currently).
' Use this functionality at your own risk.
#Const MD5_SHARED_STREAM_CACHE = False ' True

' When enabled, the needed tables/arrays are generated on runtime.
' Otherwise, this information will be precomputed and stored in the executable.
#Const MD5_GENERATE_TABLES = False ' True

' If this is enabled, this module will used an unrolled implementation of the main block processing-routine.
#Const MD5_ALTERNATE_SHIFT = False

' Enabling this will cause the main execution routine to use a more "manual" approach, by doing work without a 'For' loop.
#Const MD5_MANUAL_PROCESSING = False ' True

' Similar to 'MD5_SHARED_STREAM_CACHE', this has concurrency problems, but can potentially boost performance.
' For a thread safe version, use 'MD5_TEMPORARY_CONTAINED_OUTPUT_CACHE' instead.
#Const MD5_SHARED_OUTPUT_CACHE = False

' Since this routine uses a single object-contained block-cache, this might have concurrency issues while using the same object in multiple threads.
' This functionality should be no less safe, so this isn't exactly an extra limiter.
' Of course, this is all making the assumption that .NET doesn't already handle this stuff for you.
' I'm not a .NET guy, so I really couldn't tell you about its thread-safety standards.
' As a rule of thumb, if this is enabled, you should copy the output to a separate array (Assuming you're not using this for short-term purposes).
#Const MD5_TEMPORARY_CONTAINED_OUTPUT_CACHE = False

' Imports:

' This is used for standard 'Stream' / I/O functionality.
Imports System.IO

' Classes:
Public Class MD5
    ' Constant variable(s) (Public):
    Const HASH_SIZE As UInteger = 16

    ' The default size of the internal "stream-cache".
    Const DEFAULT_BUFFER_SIZE As UInteger = &H1000

    #If Not MD5_SHARED_STREAM_CACHE Then
        Const SHARED_BUFFER_SIZE As UInteger = 0
    #Else
        Const SHARED_BUFFER_SIZE = DEFAULT_BUFFER_SIZE
    #End If

    ' Constant variable(s) (Private):
    Private Const BLOCK_SIZE As Integer = 16 ' UInteger

    #If Not MD5_MANUAL_PROCESSING Then
        Private Const KTABLE_SIZE As Integer = 64 ' UInteger
        
        #If Not MD5_ALTERNATE_SHIFT Then
            Private Const SHIFTTABLE_SIZE As Integer = 64 ' UInteger
        #Else
            Private Const SHIFTTABLE_SIZE As Integer = 16 ' UInteger
        #End If
    #Else
        Private Const KTABLE_SIZE As Integer = 0 ' UInteger
        Private Const SHIFTTABLE_SIZE As Integer = 0 ' UInteger
    #End If

    Private Const MD5_STREAM_CACHE_MINIMUM_AREA As Integer = (BLOCK_SIZE << 2) ' UInteger

    ' Shift values (Normally, the "K" values would be here too, but there's just too many):
    #If Not MD5_ALTERNATE_SHIFT Or MD5_GENERATE_TABLES Or MD5_MANUAL_PROCESSING Then
        Private Const S1_1 As Integer = 7
        Private Const S1_2 As Integer = 12
        Private Const S1_3 As Integer = 17
        Private Const S1_4 As Integer = 22

		Private Const S2_1 As Integer = 5
		Private Const S2_2 As Integer = 9
		Private Const S2_3 As Integer = 14
		Private Const S2_4 As Integer = 20
		
		Private Const S3_1 As Integer = 4
		Private Const S3_2 As Integer = 11
		Private Const S3_3 As Integer = 16
		Private Const S3_4 As Integer = 23
		
		Private Const S4_1 As Integer = 6
		Private Const S4_2 As Integer = 10
		Private Const S4_3 As Integer = 15
		Private Const S4_4 As Integer = 21
    #End If

    ' Global variable(s) (Public):
    ' Nothing so far.

    ' Global variable(s) (Private):
    #If MD5_GENERATE_TABLES And Not MD5_MANUAL_PROCESSING Then
        Private Shared KTableGenerated As Boolean = False
        Private Shared ShiftTableGenerated As Boolean = False
    #Else
        Private Shared ReadOnly KTableGenerated As Boolean = False
        Private Shared ReadOnly ShiftTableGenerated As Boolean = False
    #End If

    ' Data tables:
    #If Not MD5_MANUAL_PROCESSING Then
        #If Not MD5_GENERATE_TABLES Then
            #If Not MD5_ALTERNATE_SHIFT Then
                Private Shared ReadOnly ShiftTable As Integer() = {S1_1, S1_2, S1_3, S1_4, S1_1, S1_2, S1_3, S1_4, S1_1, S1_2, S1_3, S1_4, S1_1, S1_2, S1_3, S1_4,
						                                           S2_1, S2_2, S2_3, S2_4, S2_1, S2_2, S2_3, S2_4, S2_1, S2_2, S2_3, S2_4, S2_1, S2_2, S2_3, S2_4,
						                                           S3_1, S3_2, S3_3, S3_4, S3_1, S3_2, S3_3, S3_4, S3_1, S3_2, S3_3, S3_4, S3_1, S3_2, S3_3, S3_4,
						                                           S4_1, S4_2, S4_3, S4_4, S4_1, S4_2, S4_3, S4_4, S4_1, S4_2, S4_3, S4_4, S4_1, S4_2, S4_3, S4_4}
            #Else
                Private Shared ReadOnly ShiftTable As Integer() = {S1_1, S1_2, S1_3, S1_4, S2_1, S2_2, S2_3, S2_4, S3_1, S3_2, S3_3, S3_4, S4_1, S4_2, S4_3, S4_4} ' {7, 12, 17, 22, 5, 9, 14, 20, 4, 11, 16, 23, 6, 10, 15, 21}
            #End If
        #Else
            'Private Shared KTable() As Integer
            Private Shared ShiftTable() As Integer
        #End If

        Private Shared ReadOnly KTable As Integer() = {&HD76AA478, &HE8C7B756, &H242070DB, &HC1BDCEEE, &HF57C0FAF, &H4787C62A, &HA8304613, &HFD469501, &H698098D8, &H8B44F7AF, &HFFFF5BB1, &H895CD7BE, &H6B901122, &HFD987193, &HA679438E, &H49B40821, &HF61E2562, &HC040B340, &H265E5A51, &HE9B6C7AA, &HD62F105D, &H2441453, &HD8A1E681, &HE7D3FBC8, &H21E1CDE6, &HC33707D6, &HF4D50D87, &H455A14ED, &HA9E3E905, &HFCEFA3F8, &H676F02D9, &H8D2A4C8A, &HFFFA3942, &H8771F681, &H6D9D6122, &HFDE5380C, &HA4BEEA44, &H4BDECFA9, &HF6BB4B60, &HBEBFBC70, &H289B7EC6, &HEAA127FA, &HD4EF3085, &H4881D05, &HD9D4D039, &HE6DB99E5, &H1FA27CF8, &HC4AC5665, &HF4292244, &H432AFF97, &HAB9423A7, &HFC93A039, &H655B59C3, &H8F0CCC92, &HFFEFF47D, &H85845DD1, &H6FA87E4F, &HFE2CE6E0, &HA3014314, &H4E0811A1, &HF7537E82, &HBD3AF235, &H2AD7D2BB, &HEB86D391}
    #End If

    ' Caches:

    ' A shared block of memory used for data-transformation.
    Private Shared block(BLOCK_SIZE - 1) As Integer

    #If MD5_SHARED_STREAM_CACHE Then
        Private Shared sharedBuffer(DEFAULT_BUFFER_SIZE - 1) As Byte
    #End If

    #If Not MD5_SHARED_OUTPUT_CACHE Then
        Private Shared sharedOutputCache(HASH_SIZE - 1) As Byte
    #End If

    ' Functions (Public):
    Shared Function RotateLeft(value As Integer, shiftBits As Integer) As Integer
        'Return (value << shiftBits) Or (value >> (32 - shiftBits))
        Return CInt((CUInt(value) << shiftBits) Or (CUInt(value) >> (32 - shiftBits)))
    End Function

    Shared Function MD5_FF(A As Integer, B As Integer, C As Integer, D As Integer, blockData As Integer, shiftValue As Integer, KValue As Integer) As Integer
        Return RotateLeft(A + MD5_F(B, C, D) + blockData + KValue, shiftValue) + B
    End Function

    Shared Function MD5_GG(A As Integer, B As Integer, C As Integer, D As Integer, blockData As Integer, shiftValue As Integer, KValue As Integer) As Integer
        Return RotateLeft(A + MD5_G(B, C, D) + blockData + KValue, shiftValue) + B
    End Function

    Shared Function MD5_HH(A As Integer, B As Integer, C As Integer, D As Integer, blockData As Integer, shiftValue As Integer, KValue As Integer) As Integer
        Return RotateLeft(A + MD5_H(B, C, D) + blockData + KValue, shiftValue) + B
    End Function

    Shared Function MD5_II(A As Integer, B As Integer, C As Integer, D As Integer, blockData As Integer, shiftValue As Integer, KValue As Integer) As Integer
        Return RotateLeft(A + MD5_I(B, C, D) + blockData + KValue, shiftValue) + B
    End Function

    ' Functions (Private):
    Private Shared Function MD5_F(X As Integer, Y As Integer, Z As Integer) As Integer
        Return ((X And Y) Or ((Not X) And Z))
    End Function

    Private Shared Function MD5_G(X As Integer, Y As Integer, Z As Integer) As Integer
        Return ((X And Z) Or (Y And (Not Z)))
    End Function

    Private Shared Function MD5_H(X As Integer, Y As Integer, Z As Integer) As Integer
        Return (X Xor Y Xor Z)
    End Function

    Private Shared Function MD5_I(X As Integer, Y As Integer, Z As Integer) As Integer
        Return (Y Xor (X Or (Not Z)))
    End Function

#If MD5_GENERATE_TABLES And Not MD5_MANUAL_PROCESSING Then
    ' This function returns 'True' if a table was generated, and 'False' if they both already exist.
    Private Shared Function GenerateTables() As Boolean
        ' Local variable(s):

        ' Allocate a response boolean/flag.
        Dim response As Boolean = False

        If (Not KTableGenerated) Then
            'KTable = New Integer(KTABLE_SIZE - 1) {}

            'For I As Integer = 0 To KTABLE_SIZE - 1 ' KTable.Length
            'KTable(I) = CInt(CUInt(Math.Abs(Math.Sin(CDbl(I) + 1.0)) * CDbl(CUInt(Math.Pow(2, 32)))))
            'Next

            ' Currently unsupported, we need to use the precomputed version.

            ' Set the table generation-flag to 'True'.
            KTableGenerated = True

            ' Set the response flag to 'True'.
            response = True
        End If

        If (Not ShiftTableGenerated) Then
            ShiftTable = New Integer(SHIFTTABLE_SIZE - 1) {}

            #If MD5_ALTERNATE_SHIFT Then
                Dim I As Integer = 0
            #End If

            #If Not MD5_ALTERNATE_SHIFT Then
                For I As Integer = 0 To 15 Step 4 ' ((SHIFTTABLE_SIZE/4)-1) Step 4
            #End If
                    ShiftTable(I) = 7
                    ShiftTable(I + 1) = ShiftTable(I) + 5
                    ShiftTable(I + 2) = ShiftTable(I + 1) + 5
                    ShiftTable(I + 3) = ShiftTable(I + 2) + 5
            #If Not MD5_ALTERNATE_SHIFT Then
                Next
            #End If

            #If Not MD5_ALTERNATE_SHIFT Then
                For I As Integer = 16 To 31 Step 4 ' ((SHIFTTABLE_SIZE/2)-1) Step 4
            #Else
                I = 4
            #End If
                    ShiftTable(I) = 5
                    ShiftTable(I + 1) = ((ShiftTable(I) + ShiftTable(I)) - 1)
                    ShiftTable(I + 2) = ShiftTable(I + 1) + ShiftTable(I)
                    ShiftTable(I + 3) = ShiftTable(I + 2) + ShiftTable(I) + 1
            #If Not MD5_ALTERNATE_SHIFT Then
                Next
            #End If

            #If Not MD5_ALTERNATE_SHIFT Then
                For I As Integer = 32 To 47 Step 4 ' (((SHIFTTABLE_SIZE/2) + (SHIFTTABLE_SIZE/4))-1) Step 4
            #Else
                I = 8
            #End If
                    ShiftTable(I) = 4
                    ShiftTable(I + 1) = ShiftTable(I) + 7
                    ShiftTable(I + 2) = ShiftTable(I + 1) + ShiftTable(I) + 1
                    ShiftTable(I + 3) = (ShiftTable(I + 2) + (ShiftTable(I) * 2) - 1)
            #If Not MD5_ALTERNATE_SHIFT Then
                Next
            #End If

            #If Not MD5_ALTERNATE_SHIFT Then
                For I As Integer = 48 To 63 Step 4 ' (SHIFTTABLE_SIZE-1) Step 4
            #Else
                I = 12
            #End If
                ShiftTable(I) = 6
                ShiftTable(I + 1) = ShiftTable(I) + 4
                ShiftTable(I + 2) = ShiftTable(I + 1) + (ShiftTable(I) - 1)
                ShiftTable(I + 3) = ShiftTable(I + 2) + ShiftTable(I)
            #If Not MD5_ALTERNATE_SHIFT Then
                Next
            #End If

            ' Set the table generation-flag to 'True'.
            ShiftTableGenerated = True

            ' Set the response flag to 'True'.
            response = True ' Return True
        End If

        ' Return the calculated response.
        Return response
    End Function
#End If

    ' Constructor(s):
    Sub New(Optional bufferSize As Integer = DEFAULT_BUFFER_SIZE)
        #If Not MD5_SHARED_STREAM_CACHE Then
            ' Allocate a new buffer for caching streams.
            Me.bufferSize = bufferSize
        #End If
    End Sub

    ' Methods:
    Function Execute(data As Stream) As Byte()
        Return Execute(data, data.Length-data.Position)
    End Function

    Function Execute(data As Stream, length As Long) As Byte()
        #If DEBUG Then
            If (length = 0) Then
                ' Throw an "end-of-stream" exception.
                Throw New EndOfStreamException()
                
                Return Nothing
            End If
        #End If

        #If MD5_GENERATE_TABLES And Not MD5_MANUAL_PROCESSING Then
            ' In the event we need to generate the tables, call this command.
            If (Not KTableGenerated OrElse Not ShiftTableGenerated) Then
                GenerateTables()
            End If
        #End If

        ' Local variable(s):

        ' The number of blocks to process.
        Dim blockCount As Long = ((length + 8) >> 6) + 1

        ' Set the initial values of our result variables:
        Dim A0 As Integer = &H67452301 ' 1732584193
        Dim B0 As Integer = &HEFCDAB89 ' -271733879
        Dim C0 As Integer = &H98BADCFE ' -1732584194
        Dim D0 As Integer = &H10325476 ' 271733878

        ' Calculate the block transformation area:
        Dim virtualBlockArraySize As Long = (blockCount * BLOCK_SIZE)

        Dim vPosition As Integer = 0

        ' A temporary variable containing the current block-ID.
        Dim blockID As Long = 0

        ' A buffer offset used for retrieving data from the internal stream-buffer.
        Dim bufferOffset As Integer = 0

        Dim finalBlockPosition As Long = (virtualBlockArraySize - BLOCK_SIZE)

        For blockIndex As Long = 0 To virtualBlockArraySize - 1 Step BLOCK_SIZE
            ' Calculate the current block-ID:
            If (blockIndex > 0) Then
                blockID = CLng(blockIndex / BLOCK_SIZE)
                bufferOffset = CInt(MD5_STREAM_CACHE_MINIMUM_AREA * blockID)
            End If

            ' Zero-out the block-cache. This can have unwanted garbage in it,
            ' even between separate uses of this command.
            Array.Clear(block, 0, block.Length)

            ' Read until the end of the stream, or the loop ends (Specified by the 'length' argument).
            #If Not MD5_SHARED_STREAM_CACHE Then
                Dim areaToRead As Integer = CInt(Math.Min(streamBuffer.Length, length)) ' bufferSize
            #Else
                Dim areaToRead As Integer = CInt(Math.Min(sharedBuffer.Length, length))
            #End If
                
                If (bufferOffset = 0 OrElse vPosition >= areaToRead) Then
                    #If MD5_SHARED_STREAM_CACHE Then
                        'Array.Clear(sharedBuffer, 0, sharedBuffer.Length)
                        
                        If (data.Read(sharedBuffer, 0, areaToRead) = 0) Then
                    #Else
                        'Array.Clear(streamBuffer, 0, streamBuffer.Length)
                        
                        If (data.Read(streamBuffer, 0, areaToRead) = 0) Then
                    #End If
                            Exit For
                        Else
                            vPosition = 0
                        End If
                End If

            Dim offsetBufferArea As Integer = CInt(Math.Min(MD5_STREAM_CACHE_MINIMUM_AREA * (blockID + 1), length))

            ' Copy the data from the message into the block's cache (In shifted/encoded form):
            For I As Integer = bufferOffset To offsetBufferArea - 1
                Dim blockPosition As Integer = ((I >> 2) Mod BLOCK_SIZE)

                #If Not MD5_SHARED_STREAM_CACHE Then
                    block(blockPosition) = block(blockPosition) Or ((CInt(streamBuffer((I - bufferOffset) + vPosition)) << ((I Mod 4) * 8)))
                #Else
                    block(blockPosition) = block(blockPosition) Or ((CInt(sharedBuffer(I - bufferOffset + vPosition)) << ((I Mod 4) * 8)))
                #End If
            Next

            ' Apply specific effects to the final block:
            If (blockIndex = finalBlockPosition) Then
                ' This position needs to be cached due to Visual Basic's lack of an "|="/"Or=" operator.
                Dim block_FinalEncode_Position_X As Integer = CInt((length >> 2) Mod BLOCK_SIZE)

                block(block_FinalEncode_Position_X) = block(block_FinalEncode_Position_X) Or (128 << (CInt(length Mod 4) * 8)) ' (MD5_STREAM_CACHE_MINIMUM_AREA*2)
                block(CInt((virtualBlockArraySize - 2) Mod BLOCK_SIZE)) = CInt(length * 8)
            End If

            vPosition += (offsetBufferArea - bufferOffset)

            Dim A = A0
            Dim B = B0
            Dim C = C0
            Dim D = D0

            #If MD5_MANUAL_PROCESSING Then
				' First round:
				A = MD5_FF(A, B, C, D, block(0), S1_1, &Hd76aa478)
				D = MD5_FF(D, A, B, C, block(1), S1_2, &He8c7b756)
				C = MD5_FF(C, D, A, B, block(2), S1_3, &H242070db)
				B = MD5_FF(B, C, D, A, block(3), S1_4, &Hc1bdceee)
				
				A = MD5_FF(A, B, C, D, block(4), S1_1, &Hf57c0faf)
				D = MD5_FF(D, A, B, C, block(5), S1_2, &H4787c62a)
				C = MD5_FF(C, D, A, B, block(6), S1_3, &Ha8304613)
				B = MD5_FF(B, C, D, A, block(7), S1_4, &Hfd469501)
				
				A = MD5_FF(A, B, C, D, block(8), S1_1, &H698098d8)
				D = MD5_FF(D, A, B, C, block(9), S1_2, &H8b44f7af)
				C = MD5_FF(C, D, A, B, block(10), S1_3, &Hffff5bb1)
				B = MD5_FF(B, C, D, A, block(11), S1_4, &H895cd7be)
				
				A = MD5_FF(A, B, C, D, block(12), S1_1, &H6b901122)
				D = MD5_FF(D, A, B, C, block(13), S1_2, &Hfd987193)
				C = MD5_FF(C, D, A, B, block(14), S1_3, &Ha679438e)
				B = MD5_FF(B, C, D, A, block(15), S1_4, &H49b40821)
				
				' Second round:
				A = MD5_GG(A, B, C, D, block(1), S2_1, &Hf61e2562)
				D = MD5_GG(D, A, B, C, block(6), S2_2, &Hc040b340)
				C = MD5_GG(C, D, A, B, block(11), S2_3, &H265e5a51)
				B = MD5_GG(B, C, D, A, block(0), S2_4, &He9b6c7aa)
				
				A = MD5_GG(A, B, C, D, block(5), S2_1, &Hd62f105d)
				D = MD5_GG(D, A, B, C, block(10), S2_2, &H02441453)
				C = MD5_GG(C, D, A, B, block(15), S2_3, &Hd8a1e681)
				B = MD5_GG(B, C, D, A, block(4), S2_4, &He7d3fbc8)
				
				A = MD5_GG(A, B, C, D, block(9), S2_1, &H21e1cde6)
				D = MD5_GG(D, A, B, C, block(14), S2_2, &Hc33707d6)
				C = MD5_GG(C, D, A, B, block(3), S2_3, &Hf4d50d87)
				B = MD5_GG(B, C, D, A, block(8), S2_4, &H455a14ed)
				
				A = MD5_GG(A, B, C, D, block(13), S2_1, &Ha9e3e905)
				D = MD5_GG(D, A, B, C, block(2), S2_2, &Hfcefa3f8)
				C = MD5_GG(C, D, A, B, block(7), S2_3, &H676f02d9)
				B = MD5_GG(B, C, D, A, block(12), S2_4, &H8d2a4c8a)
				
				' Third round:
				A = MD5_HH(A, B, C, D, block(5), S3_1, &Hfffa3942)
				D = MD5_HH(D, A, B, C, block(8), S3_2, &H8771f681)
				C = MD5_HH(C, D, A, B, block(11), S3_3, &H6d9d6122)
				B = MD5_HH(B, C, D, A, block(14), S3_4, &Hfde5380c)
				
				A = MD5_HH(A, B, C, D, block(1), S3_1, &Ha4beea44)
				D = MD5_HH(D, A, B, C, block(4), S3_2, &H4bdecfa9)
				C = MD5_HH(C, D, A, B, block(7), S3_3, &Hf6bb4b60)
				B = MD5_HH(B, C, D, A, block(10), S3_4, &Hbebfbc70)
				
				A = MD5_HH(A, B, C, D, block(13), S3_1, &H289b7ec6)
				D = MD5_HH(D, A, B, C, block(0), S3_2, &Heaa127fa)
				C = MD5_HH(C, D, A, B, block(3), S3_3, &Hd4ef3085)
				B = MD5_HH(B, C, D, A, block(6), S3_4, &H04881d05)
				
				A = MD5_HH(A, B, C, D, block(9), S3_1, &Hd9d4d039)
				D = MD5_HH(D, A, B, C, block(12), S3_2, &He6db99e5)
				C = MD5_HH(C, D, A, B, block(15), S3_3, &H1fa27cf8)
				B = MD5_HH(B, C, D, A, block(2), S3_4, &Hc4ac5665)
				
				' Fourth / Final round:
				A = MD5_II(A, B, C, D, block(0), S4_1, &Hf4292244)
				D = MD5_II(D, A, B, C, block(7), S4_2, &H432aff97)
				C = MD5_II(C, D, A, B, block(14), S4_3, &Hab9423a7)
				B = MD5_II(B, C, D, A, block(5), S4_4, &Hfc93a039)
				
				A = MD5_II(A, B, C, D, block(12), S4_1, &H655b59c3)
				D = MD5_II(D, A, B, C, block(3), S4_2, &H8f0ccc92)
				C = MD5_II(C, D, A, B, block(10), S4_3, &Hffeff47d)
				B = MD5_II(B, C, D, A, block(1), S4_4, &H85845dd1)
				
				A = MD5_II(A, B, C, D, block(8), S4_1, &H6fa87e4f)
				D = MD5_II(D, A, B, C, block(15), S4_2, &Hfe2ce6e0)
				C = MD5_II(C, D, A, B, block(6), S4_3, &Ha3014314)
				B = MD5_II(B, C, D, A, block(13), S4_4, &H4e0811a1)
				
				A = MD5_II(A, B, C, D, block(4), S4_1, &Hf7537e82)
				D = MD5_II(D, A, B, C, block(11), S4_2, &Hbd3af235)
				C = MD5_II(C, D, A, B, block(2), S4_3, &H2ad7d2bb)
				B = MD5_II(B, C, D, A, block(9), S4_4, &Heb86d391)
            #Else
                ' A temporary-variable allocated in order to transfer 'D'.
                Dim E As Integer
            
                #If Not MD5_ALTERNATE_SHIFT Then
                    Dim F As Integer = 0
                    Dim G As Integer = 0
                #End If

                For I As Integer = 0 To 63 ' 64-1 ' KTABLE_SIZE-1
                    #If Not MD5_ALTERNATE_SHIFT Then
                        If (I < 16) Then
                            'F = (B And C) Or ((Not B) And D)
                            'G = I
                            
                            F = D Xor (B And (C Xor D))
                            G = I
                        ElseIf (I < 32) Then
                            'F = (D And B) Or ((Not B) And D)
                            'G = (5 * I + 1) Mod BLOCK_SIZE
                            
                            F = C Xor (D And (B Xor C))
                            G = (5 * I + 1) Mod BLOCK_SIZE
                        ElseIf (I < 48) Then
                            'F = B Xor C Xor D
                            'G = (3 * I + 5) Mod BLOCK_SIZE
                            
                            F = B Xor C Xor D
                            G = (3 * I + 5) Mod BLOCK_SIZE
                        Else 'If (I < 64) Then
                            'F = C Xor (B Or (Not D))
                            'G = (7*I) Mod BLOCK_SIZE
                            
                            F = C Xor (B Or (Not D))
                            G = (7 * I) Mod BLOCK_SIZE
                        End If
                    #End If

                    E = D
                    D = C
                    C = B

                    #If MD5_ALTERNATE_SHIFT Then
                        If (I < 16) Then
                            B += RotateLeft(A + CInt(E Xor (C And (D Xor E))) + KTable(I) + block(I), ShiftTable(I Mod 4))
                        ElseIf (I < 32) Then
                            B += RotateLeft(A + CInt(D Xor (E And (C Xor D))) + KTable(I) + block(CInt(((5 * I + 1)) Mod BLOCK_SIZE)), ShiftTable((I Mod 4) + 4))
                        ElseIf (I < 48) Then
                            B += RotateLeft(A + CInt(C Xor D Xor E) + KTable(I) + block(CInt((3 * I + 5) Mod BLOCK_SIZE)), ShiftTable((I Mod 4) + 8))
                        Else'If (I < 64) Then
                            B += RotateLeft(A + CInt(D Xor (C Or (Not E))) + KTable(I) + block(CInt((7 * I) Mod BLOCK_SIZE)), ShiftTable((I Mod 4) + 12))
                        End If
                    #Else
                        B += RotateLeft(A + F + KTable(I) + block(G), ShiftTable(I))
                    #End If

                    A = E
                Next
            #End If

            A0 += A
            B0 += B
            C0 += C
            D0 += D
        Next

        ' Generate a temporary array containing the initial result. (Done for performance reasons)
        'Dim result As Integer() = {A0, B0, C0, D0}

        conversionCache(0) = A0
        conversionCache(1) = B0
        conversionCache(2) = C0
        conversionCache(3) = D0

        #If Not MD5_SHARED_OUTPUT_CACHE Then
            #If Not MD5_TEMPORARY_CONTAINED_OUTPUT_CACHE Then
                ' Allocate a new array of bytes to hold the output.
                Dim output(HASH_SIZE - 1) As Byte

                ' Copy the result-array to the output-array.
                Buffer.BlockCopy(conversionCache, 0, output, 0, HASH_SIZE)

                ' Return the processed version of the 'output' array.
                Return output
            #Else
                'Array.Clear(temporaryOutputCache, 0, temporaryOutputCache.Length)
                Buffer.BlockCopy(conversionCache, 0, temporaryOutputCache, 0, HASH_SIZE)
            
                Return temporaryOutputCache
            #End If
        #Else
            'Array.Clear(sharedOutputCache, 0, sharedOutputCache.Length)
            Buffer.BlockCopy(conversionCache, 0, sharedOutputCache, 0, HASH_SIZE)    
            
            Return sharedOutputCache
        #End If
    End Function

    ' Properties:

    ' By setting this property, you will be reallocate the internal stream-cache.
    ' Use this property at your own risk.
    Public Property bufferSize() As Integer
        Get
            #If Not MD5_SHARED_STREAM_CACHE Then
                Return streamBuffer.Length
            #Else
                Return sharedBuffer.Length
            #End If
        End Get

        Set(value As Integer)
            #If Not MD5_SHARED_STREAM_CACHE Then
                #If DEBUG Then
                    If (value < MD5_STREAM_CACHE_MINIMUM_AREA) Then
                        'Throw New InvalidOperationException()
                        
                        value = MD5_STREAM_CACHE_MINIMUM_AREA
                    End If
                #End If
                
                If (streamBuffer Is Nothing OrElse value <> streamBuffer.Length) Then
                    ' Allocate a new stream-cache.
                    streamBuffer = New Byte(value - 1) {}
                Else
                    ' The value was the same as the current size, simply clear the stream-cache.
                    Array.Clear(streamBuffer, 0, streamBuffer.Length)
                End If
            #Else
                ' Nothing so far.
            #End If
        End Set
    End Property

    ' Fields:
    #If Not MD5_SHARED_STREAM_CACHE Then
        Dim streamBuffer As Byte()
    #End If

    ' A small integer cache used for conversion to byte arrays.
    Dim conversionCache(3) As Integer ' 4-1 ' (HASH_SIZE/4)-1

    #If MD5_TEMPORARY_CONTAINED_OUTPUT_CACHE Then
        ' An optional cache used for single-threaded (At least ideally) TEMPORARY output.
        ' If this array is given to you, copy from it immediately.
        Dim temporaryOutputCache(HASH_SIZE - 1) As Byte
    #End If
End Class
