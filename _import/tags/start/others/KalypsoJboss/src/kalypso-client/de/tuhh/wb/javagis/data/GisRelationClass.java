package de.tuhh.wb.javagis.data;

import javax.swing.table.AbstractTableModel;

/*
import de.tuhh.wb.javagis.model.ElementSession;
import de.tuhh.wb.javagis.model.ObjectSession;
import de.tuhh.wb.javagis.model.RelationSession;
*/
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;

import java.awt.geom.Point2D;
import de.tuhh.wb.javagis.view.netview.GisPoint;

public class GisRelationClass extends GisElementClass
{
    private int mrt;//myRelationTable;
    private Hashtable relations;
    private static final int SRC_ID=0;
    private static final int SRC_CLASS=1;
    private static final int DEST_ID=2;
    private static final int DEST_CLASS=3;
    //relation-hash:
    //
    // nach RelationID sortiert
    // info Object->RelationID steht im Object
    //
    //Relations "objectView":
    //    private Vector forwardRelationsClasses; // value: keys of allowed Relation
    //    private Vector backwardRelationsClasses;

    private String myForwardLabel;
    private String myBackwardLabel;
    private String srcKeys[];
    private String destKeys[];

    public GisRelationClass(Version version,Modeldata model,int elementTable,int relationTable)
    {
	super(version,model,elementTable);
	this.mrt=relationTable;

	this.srcKeys=model.relationAllowedSrcKeys[mrt];
	this.destKeys=model.relationAllowedDestKeys[mrt];
	this.myForwardLabel=model.relationForwardLabels[mrt];
	this.myBackwardLabel=model.relationBackwardLabels[mrt];
	this.relations=new Hashtable();
    }

    public String getForwardLabel()
    {
	return myForwardLabel;
    }

    public String getBackwardLabel()
    {
	return myBackwardLabel;
    }

    public boolean isAllowedRelation(String srcKey, String destKey)
    {
	boolean srcResult=false;
	boolean destResult=false;
	for(int i=0;i<srcKeys.length;i++)
	    if(srcKeys[i].equals(srcKey))
		srcResult=true;
	for(int i=0;i<destKeys.length;i++)
	    if(destKeys[i].equals(destKey))
		destResult=true;
	return (srcResult && destResult);
    }

    private void loadRelation(Object rId)
    {
	System.out.println(getName()+" loadRelation: #"+rId.toString());
	
	System.out.println(getName()+"getRelationVector...");
	Vector relationVector=myVersion.getRelationVector(mrt,rId);
	System.out.println(getName()+"getRelationVector... done");
	Object srcId=relationVector.elementAt(0);
	String srcKey=(String)relationVector.elementAt(1);
	Object destId=relationVector.elementAt(2);
	String destKey=(String)relationVector.elementAt(3);
	
	Vector cache=new Vector();			
	System.out.println("\nGisRelationClass: "+srcId.toString()+srcKey);
	System.out.println("GisRelationClass: "+destId.toString()+destKey+"\n");
	
	cache.add(srcId); //#0
	cache.add(myVersion.getGisObjectClass(srcKey)); //#1
	cache.add(destId); //#2
	cache.add(myVersion.getGisObjectClass(destKey)); //#3
	relations.put(rId,cache);
    }

    public GisObject getSrcGisObject(Object rId)
    {
	if(!relations.containsKey(rId))
	    loadRelation(rId);
	Vector cache=(Vector)relations.get(rId);
	GisObjectClass gisObjectClass=(GisObjectClass)cache.elementAt(SRC_CLASS);
	return gisObjectClass.getGisObject(cache.elementAt(SRC_ID));
    }

    public GisObject getDestGisObject(Object rId)
    {
	if(!relations.containsKey(rId))
	    loadRelation(rId);
	Vector cache=(Vector)relations.get(rId);
	GisObjectClass gisObjectClass=(GisObjectClass)cache.elementAt(DEST_CLASS);
	return gisObjectClass.getGisObject(cache.elementAt(DEST_ID));
    }

    public GisPoint getBasePointSource(Object rId)
    {
	if(!relations.containsKey(rId))
	    loadRelation(rId);
	Vector cache=(Vector)relations.get(rId);
	GisObjectClass gisObjectClass=(GisObjectClass)cache.elementAt(SRC_CLASS);
	return gisObjectClass.getBasePoint(cache.elementAt(SRC_ID));
    }

    public GisPoint getBasePointDestination(Object rId)
    {
	if(!relations.containsKey(rId))
	    loadRelation(rId);
	Vector cache=(Vector)relations.get(rId);
	GisObjectClass gisObjectClass=(GisObjectClass)cache.elementAt(DEST_CLASS);
	return gisObjectClass.getBasePoint(cache.elementAt(DEST_ID));
    }

    public void createRelation(GisObject srcGO,GisObject destGO)
    {
	String srcKey  = srcGO.getKey();
	String destKey =destGO.getKey();
	Object srcId = srcGO.getId();
	Object destId = destGO.getId();

	System.out.println("GisRelationClass: ("+getName()+")createRelation() source #"+srcId.toString()+" ("+srcKey+")");
	System.out.println("GisRelationClass: ("+getName()+")createRelation() destination #"+destId.toString()+" ("+destKey+")");
	myVersion.createRelation(mrt,srcKey, srcId, destKey, destId);
	System.out.println("GisRelationClass: ("+getName()+")createRelation()");
	

    }

    public GisRelation getGisRelation(Object rId)
    {
	// test if it exists
	return new GisRelation(rId,this);
    }


    public void onTableElementCreate(int elementTable,Object eId)
    {
	super.onTableElementCreate(elementTable,eId);
    }
    public void onTableElementRemove(int elementTable,Object eId)
    {
	relations.remove(eId);
	super.onTableElementRemove(elementTable,eId);
    }
    public void onSimplePropertyChanged(int elementTable,Object eId)
    {
	super.onSimplePropertyChanged(elementTable,eId);
    }
}
