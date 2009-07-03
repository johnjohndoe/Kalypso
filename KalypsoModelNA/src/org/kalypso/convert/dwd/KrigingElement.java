package org.kalypso.convert.dwd;

import java.util.Vector;

import org.deegree.model.geometry.GM_Point;

public class KrigingElement 
{
	final GM_Point m_centerPoint;
	final Vector m_relations=new Vector();
	
	public KrigingElement(GM_Point centerpoint)
	{
		m_centerPoint=centerpoint;		
	}
	
	public void addRelation(double factor,String id)
	{
		m_relations.add(new KrigingRelation(factor,id));
	}
	
	public KrigingRelation[] getRelations()
	{
		return (KrigingRelation[]) m_relations.toArray(new KrigingRelation[m_relations.size()]);
	}

	public GM_Point getCenterPoint()
	{
		return m_centerPoint;
	}
	
	public int getSize()
	{
		return m_relations.size();
	}
	
	
}
