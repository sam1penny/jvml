package org.example;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;

public class LocalClassWriter extends ClassWriter {
    public LocalClassWriter(int flags) {
        super(flags);
    }

    public LocalClassWriter(ClassReader classReader, int flags) {
        super(classReader, flags);
    }

    @Override
    protected String getCommonSuperClass(String type1, String type2) {
        if (type1.contains("Lambda$") || type2.contains("Lambda$")) {
            return "java/util/function/Function";
        }
        return super.getCommonSuperClass(type1, type2);
    }


}
