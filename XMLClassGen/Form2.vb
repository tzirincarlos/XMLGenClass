Public Class Form2

    Private Class clsItem
        Public Nombre As String
        Public Tipo As Integer
        Public TipoNombre As String
    End Class

    Private items As New List(Of clsItem)

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        TextBox1.Clear()
        TextBox2.Clear()
    End Sub

    Private Sub Generar()
        Dim tp As Integer
        Dim sn, st, stt, s As String
        Dim item As clsItem
        Dim rc As Integer = 0

        TextBox2.Text = ""

        Try

            rc = TextBox1.Lines.Count
            items.Clear()

            For i = 0 To rc - 1
                s = TextBox1.Lines(i)
                If s <> "" Then

                End If

            Next

        Catch ex As Exception
            MsgBox(ex.Message)
        End Try

    End Sub

End Class