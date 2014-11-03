' NOTES:
' ATTENTION: For the sake of seeking, caching, and fair comparison, this demo loads the entire file into RAM.

' Preprocessor related:
#Const COMPARISON_DEMO_PROFILE = False ' True
#Const COMPARISON_DEMO_USE_MEMORYSTREAM = True ' False

#If Not COMPARISON_DEMO_PROFILE
    #Const COMPARISON_DEMO_OUTPUT = True
#End If

#Const COMPARISON_DEMO_LOGTIMES = COMPARISON_DEMO_OUTPUT

'#Const MD5_TEMPORARY_CONTAINED_OUTPUT_CACHE = True
'#Const MD5_SHARED_OUTPUT_CACHE = True

' Imports:
Imports System.IO

#If COMPARISON_DEMO_LOGTIMES
    Imports System.Diagnostics
#End If

Imports System.Security.Cryptography

' Classes:
Public Class Main
    ' Functions:
    Shared Sub Main() ' args As String()
        Dim testCount As ULong = 1000000

        #If COMPARISON_DEMO_LOGTIMES
            Dim watch As New Stopwatch
        #End If

        Dim fileLocation As String = "Hello world.txt" ' "Test.c"
        Dim hash As Byte()

        #If COMPARISON_DEMO_USE_MEMORYSTREAM
            Dim fileBytes As Byte() = File.ReadAllBytes(fileLocation)
        #End If

        #If COMPARISON_DEMO_OUTPUT
            Console.WriteLine("Example: Standard implementation vs. Custom implementation.")
            Console.WriteLine("Calculating the hash of the file '" & fileLocation & "' " & testCount & If(testCount=1, " time", " times") & "...")
        #End If

        Dim fileStream As Stream

        #If COMPARISON_DEMO_USE_MEMORYSTREAM
            fileStream = New MemoryStream(fileBytes)
        #Else
            fileStream = New FileStream(fileLocation, FileMode.Open)
        #End IF

        Dim customMD5 As MD5 = New MD5(CInt(fileStream.Length))

        #If COMPARISON_DEMO_LOGTIMES
            'watch.Reset()
            watch.Start()
        #End If

        For I As ULong = 1 To testCount - 1UL
            customMD5.Execute(fileStream, fileStream.Length)

            ' Reset the stream position.
            fileStream.Position = 0
        Next

        hash = customMD5.Execute(fileStream, fileStream.Length)

        #If COMPARISON_DEMO_LOGTIMES
            watch.Stop()
        #End If

        #If COMPARISON_DEMO_OUTPUT
            #If COMPARISON_DEMO_LOGTIMES
                Console.WriteLine("That took " & watch.ElapsedMilliseconds & " milliseconds.")
            #End If
            
            Console.WriteLine("Hash: " & BitConverter.ToString(hash).Replace("-", ""))
        #End If

        ' Reset the stream position.
        fileStream.Position = 0

        #If COMPARISON_DEMO_LOGTIMES
            watch.Reset()
        #End If

        Dim standardMD5 = New MD5CryptoServiceProvider()

        #If COMPARISON_DEMO_LOGTIMES
            watch.Start()
        #End If

        For I As ULong = 1 To testCount - 1UL
            standardMD5.ComputeHash(fileStream)

            ' Reset the stream position.
            fileStream.Position = 0
        Next

        hash = standardMD5.ComputeHash(fileStream)

        #If COMPARISON_DEMO_LOGTIMES
            watch.Stop()
        #End If

        ' Close the file-stream:
        fileStream.Close(): fileStream = nothing

        #If COMPARISON_DEMO_OUTPUT
            #If COMPARISON_DEMO_LOGTIMES
                Console.WriteLine("That took " & watch.ElapsedMilliseconds & " milliseconds.")
            #End If
            
            Console.WriteLine("Hash: " & BitConverter.ToString(hash).Replace("-", ""))
            
            Console.WriteLine()
            Console.Write("Waiting for user-input in order to close...")
            Console.ReadLine()
        #End If

        Return
    End Sub
End Class
