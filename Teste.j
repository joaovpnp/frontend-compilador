.class public Teste
.super java/lang/Object

.field public static read Ljava/util/Scanner;

.method public <init>()V
	.limit stack 1
	.limit locals 1
	aload_0
	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method static <clinit>()V
	.limit stack 3
	.limit locals 0
	new java/util/Scanner
	dup
	getstatic java/lang/System/in Ljava/io/InputStream;
	invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V
	putstatic Teste/read Ljava/util/Scanner;
	return
.end method

.method public static imprimir(Ljava/lang/String;D)V
	.limit stack 20
	.limit locals 3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	aload_0
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	dload_1
	invokevirtual java/io/PrintStream/println(D)V
	return
.end method

.method public static calcular(ID)I
	.limit stack 20
	.limit locals 4
	iconst_0
	istore_3
	iload_0
	bipush 10
	if_icmpge L10
	goto L8
L10:
	dload_1
	dconst_0
	dcmpg
	ifne L7
	goto L8
L7:
	iload_0
	iconst_2
	imul
	istore_3
	goto L9
L8:
	iload_0
	iconst_5
	iadd
	istore_3
L9:
	iload_3
	ireturn
.end method

.method public static exibirMensagem()V
	.limit stack 20
	.limit locals 1
	aconst_null
	astore_0
	ldc "Processamento concluido!"
	astore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	aload_0
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	return
.end method

.method public static main([Ljava/lang/String;)V
	.limit stack 20
	.limit locals 4

	iconst_0
	istore_0
	dconst_0
	dstore_1
	aconst_null
	astore_3
	iconst_0
	istore_0
	ldc2_w 10.5
	dstore_1
	ldc "Iniciando o programa"
	astore_3
	getstatic java/lang/System/out Ljava/io/PrintStream;
	aload_3
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	ldc "Entrada contador:"
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	getstatic Teste/read Ljava/util/Scanner;
	invokevirtual java/util/Scanner/nextInt()I
	istore_0
L0:
	iload_0
	bipush 100
	if_icmplt L1
	goto L3
L3:
	dload_1
	dconst_1
	dcmpg
	ifle L2
	goto L1
L1:
	iload_0
	iconst_1
	iadd
	istore_0
	dload_1
	ldc2_w 2.0
	ddiv
	dstore_1
	iload_0
	bipush 50
	if_icmpeq L4
	goto L5
L4:
	iload_0
	bipush 10
	imul
	istore_0
	goto L6
L5:
L6:
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	dload_1
	invokevirtual java/io/PrintStream/println(D)V
	goto L0
L2:
	iload_0
	dload_1
	ldc 1000000
	i2d
	dadd
	invokestatic Teste/calcular(ID)I
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	invokestatic Teste/exibirMensagem()V
	ldc "Teste cast int (1) to double"
	iconst_1
	i2d
	invokestatic Teste/imprimir(Ljava/lang/String;D)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	ldc "Teste cast double (3.4) to int"
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	ldc2_w 3.4
	d2i
	istore_0
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload_0
	invokevirtual java/io/PrintStream/println(I)V
	return
.end method

