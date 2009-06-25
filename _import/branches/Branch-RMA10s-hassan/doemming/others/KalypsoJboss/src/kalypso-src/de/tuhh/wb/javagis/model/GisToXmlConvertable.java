package de.tuhh.wb.javagis.model;

import java.io.PrintWriter;
import de.tuhh.wb.javagis.xml.VectorSet;
public abstract interface GisToXmlConvertable
{
    public void toXml(PrintWriter out);
    public VectorSet toVectorSetTransferObject();
}
