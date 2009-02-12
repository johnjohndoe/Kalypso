
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

public interface RelationSession extends ElementSession
{
    /**
     * Creates a new Object in the database
     * @return PrimaryKey
     */
    public Object createRelation(String srcKey,Object srcId,String destKey,Object destId) throws CreateException, java.rmi.RemoteException;

    public String[] relationSourceKeys() throws java.rmi.RemoteException;

    public String[] relationDestinationKeys()  throws java.rmi.RemoteException;
    
    public Vector getRelationVector(Object rId) throws javax.ejb.FinderException,java.rmi.RemoteException;

    //   public void removeObject( java.lang.Integer id ) throws javax.ejb.RemoveException, javax.ejb.FinderException, java.rmi.RemoteException;
    
}
