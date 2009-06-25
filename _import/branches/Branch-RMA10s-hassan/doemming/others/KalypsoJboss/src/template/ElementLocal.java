
package de.tuhh.wb.javagis.model;

import java.lang.*;
import javax.ejb.CreateException;
import javax.ejb.EntityBean;
import javax.ejb.EntityContext;
import javax.ejb.FinderException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import de.tuhh.wb.javagis.property.ChartProperty;
import de.tuhh.wb.javagis.property.VectorSet;
import de.tuhh.wb.javagis.model.VersionLocal;
import de.tuhh.wb.javagis.model.GisTransferObject;

public interface ElementLocal extends javax.ejb.EJBLocalObject
{
    public VersionLocal getVersion();
    public void setVersion(VersionLocal version);
    
    public java.lang.Integer getId();
    public void setId( java.lang.Integer objectId);

    public java.lang.String getInfo();

    public void loadGisTransferObject(GisTransferObject transferObject);
}
