package de.tuhh.wb.javagis.model;


import java.lang.*;
import java.rmi.RemoteException;
import javax.ejb.EJBObject;
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
import org.xml.sax.SAXException;
import java.io.IOException;
import de.tuhh.wb.javagis.model.BasePointTransfer;


import de.tuhh.wb.javagis.property.VectorSet;

import de.tuhh.wb.javagis.xml.GisTransferObject;
import java.io.PrintWriter;
 
public interface VersionSession extends EJBObject
{
    public java.lang.Object createVersion(String project,String name,String state,java.lang.String description,java.lang.String history ) throws javax.ejb.CreateException, java.rmi.RemoteException;
    public void removeVersion(Object vId ) throws RemoveException, FinderException,  RemoteException;
    public void renameVersion(Object vId,String project,String name,String state,String description,String hostory) throws FinderException, RemoteException;

    // return number of versions
    public int getSize(  ) throws FinderException, RemoteException;
    // returns number of elements in Version
    public int getNumberOfObjects( Object vId ) throws FinderException, RemoteException;

    public String getVersionProject(Object vId)  throws  RemoteException,  FinderException;
    public String getVersionState(Object vId)  throws  RemoteException,  FinderException;
    public String getVersionName(Object vId)  throws  RemoteException,  FinderException;
    public String getVersionDescription(Object vId)  throws  RemoteException,  FinderException;
    public String getVersionHistory(Object vId)  throws  RemoteException,  FinderException;

    public void   setVersionProject(Object vId,String project)  throws  RemoteException,  FinderException;
    public void   setVersionState(Object vId,String state)  throws  RemoteException,  FinderException;
    public void   setVersionName(Object vId,String name)  throws  RemoteException,  FinderException;
    public void   setVersionDescription(Object vId,String descrition)  throws  RemoteException,  FinderException;
    public void   setVersionHistory(Object vId,String history)  throws  RemoteException,  FinderException;

    public java.util.Vector getPrimaryKeyList(  ) throws  FinderException,  RemoteException;

    public void importFromXml( java.lang.Object vId,java.io.File file ) throws NamingException,  CreateException,  FinderException, IOException, SAXException,  RemoteException;
    public void exportToXml( java.lang.Object vId,java.io.File file ) throws NamingException, IOException,  CreateException,  RemoteException,FinderException;
    public void copyVersion(Object vId,String project,String name,String state,String description,String hostory) throws NamingException,CreateException,FinderException,SAXException, RemoteException;

    public void run(String command) throws  RemoteException;

    //model-data:
    // Elements:
    public String[] getElementKeys() throws RemoteException;
    public String[] getObjectKeys() throws  RemoteException;
    public String[] getRelationKeys() throws  RemoteException;
    public String[] getElementNames() throws  RemoteException;
    public String[] getElementSymbolNames() throws  RemoteException;
    public String[] getElementDescriptions() throws  RemoteException;

    //Properies:
    public String[] elementGetSimplePropertyKeys(int elementTable) throws  RemoteException;
    public String[] elementGetSimplePropertyNames(int elementTable) throws  RemoteException;
    public String[] elementGetSimplePropertyDescriptions(int elementTable) throws  RemoteException;
    public String[] elementGetSimplePropertyUnits(int elementTable) throws  RemoteException;
    public Class[] elementGetSimplePropertyClasses(int elementTable) throws  RemoteException;
    public String[] elementGetSimplePropertyFormats(int elementTable) throws  RemoteException;
    public String[] elementGetVectorSetKeys(int elementTable) throws  RemoteException;
    public String[] elementGetVectorSetNames(int elementTable) throws  RemoteException;
    public String[] elementGetVectorSetDescriptions(int elementTable) throws  RemoteException;

    public boolean[] elementIsRelation() throws  RemoteException;
    public String[] relationGetForwardLabels() throws  RemoteException;
    public String[] relationGetBackwardLabels() throws  RemoteException;
    public String[] relationGetSourceKeys(int relationTable) throws  RemoteException;
    public String[] relationGetDestinationKeys(int relationTable) throws  RemoteException;

    public String[] objectForwardRelations(int objectTable) throws  RemoteException;
    public String[] objectBackwardRelations(int objectTable) throws  RemoteException;


    //PropertyAccess:
    public Vector getPrimaryKeyList(int elementTable,Object vId) throws FinderException,  RemoteException;
    public void elementSetVectorSet(int elementTable,Object eId,int pos,Object value) throws FinderException, RemoteException;
    public Object elementGetVectorSet(int elementTable,Object eId,int pos) throws FinderException, RemoteException;
    public void elementSetVectorSets(int elementTable,Object eId,Vector vectorSets) throws FinderException, RemoteException;
    public Vector elementGetVectorSets(int elementTable,Object eId) throws FinderException, RemoteException;


    public Hashtable elementGetBasePoints(int pos,Vector oIds) throws FinderException, RemoteException;
    public BasePointTransfer elementGetBasePoint(int pos,Object oId) throws FinderException, RemoteException;
    public void elementSetBasePoint(int pos,Object oId, BasePointTransfer point) throws FinderException, RemoteException;
    public void elementSetSimpleProperty(int table,Object oId, int pos,Object value) throws FinderException, RemoteException;
    public Object elementGetSimpleProperty(int elementTable,Object oId, int pos) throws FinderException, RemoteException;
    public Vector elementGetSimplePropertyRow(int elementTable,Object oId) throws FinderException, RemoteException;
    public Hashtable elementGetSimplePropertyRows(int elementTable,Vector eIds) throws FinderException, RemoteException;

    
    public Vector getRelationVector(int relationTable,Object rId) throws FinderException, RemoteException;


    public Vector objectGetForwardRelations(int objectTable,Object oId) throws FinderException, RemoteException;
    public Vector objectGetBackwardRelations(int objectTable,Object oId) throws FinderException, RemoteException;

    public Object objectCreate(int objectTable,Object vId) throws CreateException, RemoteException;
    public Object createRelation(int relationTable,Object vId,String srcKey,Object srcId,String destKey,Object destId) throws CreateException,FinderException, RemoteException;
    public void removeElement(int elementTable,Object eId) throws RemoveException, FinderException, RemoteException;
}
