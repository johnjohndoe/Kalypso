package de.tuhh.wb.javagis.model;

import java.io.Writer;
import java.io.IOException;
import java.io.PrintWriter;
import de.tuhh.wb.javagis.xml.VectorSet;
public abstract interface GisToXmlConvertable
{
    public void toXml(Writer out) throws IOException;
    public VectorSet toVectorSetTransferObject();
}
