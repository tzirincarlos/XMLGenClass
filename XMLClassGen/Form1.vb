Public Class Form1

    Private Class clsItem
        Public Nombre As String
        Public Tipo As Integer
        Public TipoNombre As String
    End Class

    Private items As New List(Of clsItem)
    Private iitems As New List(Of String)
    Private ritems As New List(Of String)

    Private clsName, s, ss, t, tt As String
    Private rc, pp As Integer

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Label1.Text = ""
        TextBox1.Text = ""
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Parse()
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        If (TextBox2.Text = "") Then
            MsgBox("No está definida estructura de clase") : Return
        End If
        Generate()
    End Sub

#Region " Codigo VB "

    Private Sub Parse()

        ritems.Clear() : clsName = ""

        rc = TextBox1.Lines.Count
        If (rc = 0) Then
            MsgBox("Falta pegar el codigo de la clase del Visual Studio") : Return
        End If

        For i = 0 To rc - 1
            s = TextBox1.Lines(i)

            pp = s.IndexOf("Public Class")
            If (pp >= 0) Then
                ss = Mid(s, pp + 13)
                clsName = ss.Trim
            Else

                pp = s.IndexOf("Public Property")
                If (pp >= 0) Then
                    ss = Mid(s, pp + 16)
                    ss = ss.Replace("()", "")
                    ss = ss.Replace("As", "-")
                    ss = ss.Replace(" New ", "  ")
                    pp = ss.IndexOf("=")
                    If (pp >= 0) Then ss = Mid(ss, 1, pp)
                    ss = ss.Trim

                    ritems.Add(ss)
                End If
            End If

        Next

        If (clsName = "") Then
            MsgBox("No se logro determinar nombre de la clase") : Return
        End If

        If (ritems.Count = 0) Then
            MsgBox("La clase está vacia") : Return
        End If

        Label1.Text = clsName
        TextBox2.Clear()

        For i = 0 To ritems.Count - 1
            TextBox2.Text &= ritems(i) & vbCrLf
        Next

        TabControl1.SelectTab(1)
    End Sub

#End Region

#Region " Estructura "

    Private Sub Generate()
        Dim tp As Integer
        Dim sn, st, stt As String
        Dim item As clsItem
       

        TextBox3.Text = "" : TextBox4.Text = ""
        rc = TextBox2.Lines.Count
        items.Clear()

        For i = 0 To rc - 1

            s = TextBox2.Lines(i)

            If (s <> "") Then

                pp = s.IndexOf("-")
                If (pp < 2) Then
                    MsgBox("Valor incorrecto : " + s) : Return
                End If

                sn = Mid(s, 1, pp - 1) : sn = sn.Trim
                st = Mid(s, pp + 3) : st = st.Trim : stt = st

                pp = st.IndexOf("List")
                If (pp >= 0) Then
                    tp = 0
                    st = st.Replace("List", "")
                    st = st.Replace("(", "")
                    st = st.Replace(")", "")
                    st = st.Replace("Of", "")
                    st = st.Trim
                Else
                    tp = -1
                    Select Case st
                        Case "String"
                            st = "String" : tp = 1
                        Case "Integer"
                            st = "int" : tp = 2
                        Case "Double"
                            st = "double" : tp = 3
                        Case "Date"
                            st = "String" : tp = 4
                        Case "Datetime"
                            st = "String" : tp = 5
                        Case "Boolean"
                            st = "boolean" : tp = 6
                    End Select
                End If

                item = New clsItem

                item.Nombre = sn
                item.Tipo = tp
                item.TipoNombre = st

                items.Add(item)

            End If
        Next

        buildClass()
        TabControl1.SelectTab(2)

    End Sub

#End Region

#Region "Clase "

    Private Sub buildClass()

        TextBox3.Clear() : ss = ""
        TextBox4.Clear() : tt = ""

        Try
            buildHeader()

            buildDeclaration()
            buildConstructor()
            buildSetsGets()

            buildFooter()

            TextBox3.Text = ss
            TextBox4.Text = tt

            My.Computer.Clipboard.SetText(TextBox3.Text)
        Catch ex As Exception
            MsgBox(ex.Message)
        End Try

    End Sub

    Private Sub buildHeader()
        ads()
        ads("import org.simpleframework.xml.Element;")
        ads()
        ads("public class " & clsName & " {")
        ads()

        ' Auxiliar Class

        adt()
        adt()
        adt("//  " & clsName & "List")
        adt()
        adt()
        adt("import org.simpleframework.xml.ElementList;")
        adt()
        adt("import java.util.List;")
        adt()
        adt("public class " & clsName & "List {")
        adt("@ElementList(inline=true" & ",required=false)")
        adt("public List<" & clsName & "> items;")
        adt("}")
        adt()
        adt()
        adt("//--------------------------------------------------------")
        adt()

    End Sub

    Private Sub buildDeclaration()
        For i = 0 To items.Count - 1

            Select Case items(i).Tipo
                Case > 0
                    addPrimitive(items(i))
                Case 0
                    addList(items(i))
                Case -1
                    addClass(items(i))
            End Select
        Next

        ads()

    End Sub

    Private Sub addPrimitive(itm As clsItem)

        If itm.Tipo = 1 Then
            s = "    @Element(required=false) public " & itm.TipoNombre & " " & itm.Nombre & "=""""" & ";"
        ElseIf itm.Tipo = 2 Then
            s = "    @Element(required=false) public " & itm.TipoNombre & " " & itm.Nombre & "=0" & ";"
        ElseIf itm.Tipo = 3 Then
            s = "    @Element(required=false) public " & itm.TipoNombre & " " & itm.Nombre & "=0" & ";"
        ElseIf itm.Tipo = 4 Then
            s = "    @Element(required=false) public " & itm.TipoNombre & " " & itm.Nombre & "=" & """1900-01-01T00:00:01""" & ";"
        ElseIf itm.Tipo = 5 Then
            s = "    @Element(required=false) public " & itm.TipoNombre & " " & itm.Nombre & "=" & """1900-01-01T00:00:01""" & ";"
        ElseIf itm.Tipo = 6 Then
            s = "    @Element(required=false) public " & itm.TipoNombre & " " & itm.Nombre & "=false" & ";"
        End If

        ads(s)
    End Sub

    Private Sub addClass(itm As clsItem)
        s = "    @Element(required=false) public " & itm.TipoNombre & " " & itm.Nombre & "=new " & itm.TipoNombre & "();"
        ads(s)
    End Sub

    Private Sub addList(itm As clsItem)
        Dim cn As String = itm.TipoNombre

        s = "    @Element(required=false) public " & itm.TipoNombre & "List " & itm.Nombre & "=new " & itm.TipoNombre & "List();"
        ads(s)

        ' Auxiliar Class

        adt()
        adt()
        adt("//  " & cn & "List")
        adt()
        adt()
        adt("import org.simpleframework.xml.ElementList;")
        adt()
        adt("import java.util.List;")
        adt()
        adt("public class " & cn & "List {")
        adt("@ElementList(inline=true" & ",required=false)")
        adt("public List<" & cn & "> items;")
        adt("}")
        adt()
        adt()
        adt("//--------------------------------------------------------")
        adt()

    End Sub

    Private Sub buildConstructor()
        Dim ii As Integer = 1

        ads()
        ads("    public " & clsName & "() {")
        ads("    }")
        ads()

        s = "    public " & clsName & "("
        For i = 0 To items.Count - 1
            If (items(i).Tipo = 0) Then
                s = s & items(i).TipoNombre & "List " & items(i).Nombre
            Else
                s = s & items(i).TipoNombre & " " & items(i).Nombre
            End If
            If i <> items.Count - 1 Then s = s & ","

            ii += 1
            If (ii = 5) Then
                ii = 1 : ads(s) : s = "            "
            End If
        Next
        ads(s & ") {")
        ads()

        For i = 0 To items.Count - 1
            ads("        this." & items(i).Nombre & "=" & items(i).Nombre & ";")
        Next

        ads()
        ads("    }")
        ads()
    End Sub

    Private Sub buildSetsGets()
        ads()

        For i = 0 To items.Count - 1
            If (items(i).Tipo <> 0) Then
                ads("    public " & items(i).TipoNombre & " get" & items(i).Nombre & "() {")
            Else
                ads("    public " & items(i).TipoNombre & "List get" & items(i).Nombre & "() {")
            End If
            ads("        return " & items(i).Nombre & ";")
            ads("    } ")

            If (items(i).Tipo <> 0) Then
                ads("    public void set" & items(i).Nombre & "(" & items(i).TipoNombre & " value) {")
            Else
                ads("    public void set" & items(i).Nombre & "(" & items(i).TipoNombre & "List value) {")
            End If
            ads("        " & items(i).Nombre & "=value;")
            ads("    } ")

        Next

        ads()
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        My.Computer.Clipboard.SetText(TextBox3.Text)
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        TextBox1.Clear()
    End Sub

    Private Sub buildFooter()
        ads("}")
        ads()
    End Sub

#End Region

#Region " Aux "

    Private Sub ads(str As String)
        ss &= str & vbCrLf
    End Sub

    Private Sub ads()
        ss &= "" & vbCrLf
    End Sub

    Private Sub adt(str As String)
        tt &= str & vbCrLf
    End Sub

    Private Sub adt()
        tt &= "" & vbCrLf
    End Sub

#End Region

End Class
