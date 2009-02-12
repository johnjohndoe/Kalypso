
package de.tuhh.wb.javagis.model;

import java.lang.*;
import javax.ejb.CreateException;
import javax.ejb.RemoveException;
import javax.ejb.FinderException;
import javax.ejb.EJBException;
import javax.ejb.SessionBean;
import javax.ejb.SessionContext;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import java.util.Vector;
import java.util.Hashtable;
import java.util.Collection;
import java.util.Iterator;
import de.tuhh.wb.javagis.property.ChartProperty;
import de.tuhh.wb.javagis.property.VectorSet;
import de.tuhh.wb.javagis.model.IdManagerLocal;
import de.tuhh.wb.javagis.model.IdManagerLocalHome;

public interface ObjectSession extends ElementSession
{
    /**
     * Creates a new Object in the database
     * @return PrimaryKey
     */
    public java.lang.Object createObject(  ) throws javax.ejb.CreateException, java.rmi.RemoteException;

    public String[] backwardRelations() throws java.rmi.RemoteException;

    public String[] forwardRelations()  throws java.rmi.RemoteException;
    
    public boolean hasSymbol() throws java.rmi.RemoteException;
    public void setBasePointX(Object primKey,Double x) throws javax.ejb.FinderException, java.rmi.RemoteException;
    public Double getBasePointX(Object primKey) throws javax.ejb.FinderException, java.rmi.RemoteException;
    public void setBasePointY(Object primKey,Double y) throws javax.ejb.FinderException, java.rmi.RemoteException;
    public Double getBasePointY(Object primKey) throws javax.ejb.FinderException, java.rmi.RemoteException;

    /*    public java.lang.String getInfo( java.lang.Object primKey ) throws javax.ejb.FinderException, java.rmi.RemoteException;
    
    public java.lang.String getInfoForAll( java.lang.Object primKey ) throws javax.ejb.FinderException, java.rmi.RemoteException;
    
    public java.util.Vector getPrimaryKeyList(  ) throws java.rmi.RemoteException;
    
    public int getSize(  ) throws java.rmi.RemoteException;
    
    // Meta-tags:
    public String name() throws java.rmi.RemoteException;
    public String description() throws java.rmi.RemoteException;
    public String key() throws java.rmi.RemoteException;

   public void removeObject( java.lang.Integer id ) throws javax.ejb.RemoveException, javax.ejb.FinderException, java.rmi.RemoteException;
    */
}
