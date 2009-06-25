
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
import de.tuhh.wb.javagis.model.GisTransferObject;
import java.io.PrintWriter;

public interface ElementSession extends javax.ejb.EJBObject
{
    public java.lang.String getInfo( java.lang.Object primKey ) throws javax.ejb.FinderException, java.rmi.RemoteException;
    
    public java.lang.String getInfoForAll( java.lang.Object primKey ) throws javax.ejb.FinderException, java.rmi.RemoteException;
    
    public java.util.Vector getPrimaryKeyList(  ) throws java.rmi.RemoteException;
    //    public java.util.Vector getPrimaryKeyListOrdered(int colIndex) throws javax.ejb.FinderException,java.rmi.RemoteException;
    
    public int getSize(  ) throws java.rmi.RemoteException;
    public void toXML(String fileName) throws java.io.IOException,java.rmi.RemoteException;
    
    // Meta-tags:
    public String name() throws java.rmi.RemoteException;
    public String description() throws java.rmi.RemoteException;
    public String key() throws java.rmi.RemoteException;
    public boolean isRelation() throws java.rmi.RemoteException;

    public String[] simplePropertyKeys() throws java.rmi.RemoteException;
    public String[] chartPropertyKeys() throws java.rmi.RemoteException;
    public String[] vectorSetKeys() throws java.rmi.RemoteException;


    public String[] simplePropertyNames() throws java.rmi.RemoteException;
    public String[] chartPropertyNames() throws java.rmi.RemoteException;
    public String[] vectorSetNames() throws java.rmi.RemoteException;

    public String[] simplePropertyDescriptions() throws java.rmi.RemoteException;
    public String[] chartPropertyDescriptions() throws java.rmi.RemoteException;
    public String[] vectorSetDescriptions() throws java.rmi.RemoteException;

    public Class[] simplePropertyClasses() throws java.rmi.RemoteException;

    public void removeObject( java.lang.Integer id ) throws javax.ejb.RemoveException, javax.ejb.FinderException, java.rmi.RemoteException;


    // methods from TableModel:
    /*
      public int getSimplePropertySize() throws java.rmi.RemoteException;
      public int getChartPropertySize() throws java.rmi.RemoteException;
      public int getVectorSetSize() throws java.rmi.RemoteException;

      public String getSimplePropertyName(int col) throws java.rmi.RemoteException;
      public String getChartPropertyName(int col) throws java.rmi.RemoteException;
      public String getVectorSetName(int col) throws java.rmi.RemoteException;

      public Class getSimplePropertyClass(int col) throws java.rmi.RemoteException;
    */
    public Object getSimplePropertyValue(Object primKey,int colIndex)  throws java.rmi.RemoteException, javax.ejb.FinderException;
    public void   setSimplePropertyValue(Object primKey,int colIndex, Object value)  throws java.rmi.RemoteException, javax.ejb.FinderException;

    public Vector getSimplePropertyRow(Object primKey)  throws java.rmi.RemoteException, javax.ejb.FinderException;
    public Hashtable getSimplePropertyRows(Vector primKeys)  throws java.rmi.RemoteException, javax.ejb.FinderException;

    public void loadGisTransferObject(Object primKey,GisTransferObject transferObject) throws javax.ejb.FinderException,java.rmi.RemoteException;

}
