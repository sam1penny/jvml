package org.example;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/*
credit - https://stackoverflow.com/questions/46622206/any-way-to-regenerate-stackmap-from-byte-code
*/

public class StackMapGen {
    public static void main(String[] args) {
        if (args.length == 0) {
            System.out.println("supply argument of class file!");
            return;
        }
        String filename = args[0];
        try {
            byte[] bytecode = Files.readAllBytes(Path.of(filename));
            ClassReader cr = new ClassReader(bytecode);
// passing cr to ClassWriter to enable optimizations
            ClassWriter cw = new ClassWriter(cr, ClassWriter.COMPUTE_FRAMES);
            cr.accept(new ClassVisitor(Opcodes.ASM5, cw) {
                @Override
                public MethodVisitor visitMethod(int access, String name, String desc,
                                                 String signature, String[] exceptions) {
                    MethodVisitor writer=super.visitMethod(access, name, desc, signature, exceptions);
                    return new MethodVisitor(Opcodes.ASM5, writer) {
                        // not changing anything, just preventing code specific optimizations
                    };
                }
            }, ClassReader.SKIP_FRAMES);
            bytecode = cw.toByteArray(); // with recalculated stack maps
            try (FileOutputStream fos = new FileOutputStream(filename)) {
                fos.write(bytecode);
                //fos.close(); There is no more need for this line since you had created the instance of "fos" inside the try. And this will automatically close the OutputStream
            }

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}