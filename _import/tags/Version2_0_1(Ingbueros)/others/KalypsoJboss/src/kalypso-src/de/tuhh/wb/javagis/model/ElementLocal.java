package de.tuhh.wb.javagis.model;

import java.util.Vector;

import de.tuhh.wb.javagis.xml.GisTransferObject;

public interface ElementLocal extends javax.ejb.EJBLocalObject
{
    public VersionLocal getVersion();
    public void setVersion(VersionLocal version);
    
    public java.lang.Integer getId();
    public void setId( java.lang.Integer objectId);

    public java.lang.String getInfo();

    public void loadGisTransferObject(GisTransferObject transferObject);

    public BasePointTransfer getBasePoint();
    public void setBasePoint(BasePointTransfer pos);

    public void setVectorSet(int pos,Object value);
    public Object getVectorSet(int pos);
    public void setVectorSets(Vector vectorSets);
    public Vector getVectorSets();

    public Object getSimpleProperty(int pos);
    public void setSimpleProperty(int pos, Object value);
    public Vector getSimplePropertyRow();
    public GisTransferObject toGisTransferObject();
}
