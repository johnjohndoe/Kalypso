package org.kalypso.afgui.model.impl;

import org.kalypso.afgui.model.EActivityRelationship;
import org.kalypso.afgui.model.IActivitySpecification;
import org.kalypso.afgui.model.IRelationshipStatement;

/**
 * Represents a relationship between 2 activities.
 * The class basically makes the following statement:<br/>
 * subject pridicate object <br/>.
 * 
 * <code>new RelationshipStatement(sub,ActivityRelatioship.HAS_A)</code>
 * 
 * @author Patrice Congo
 */
public class RelationshipStatement implements IRelationshipStatement
{
	/**
	 * The activity playing the subject role in the statement 
	 */
	final private IActivitySpecification subjectSpec;
	
	/**
	 * The predicate descripbing the statement nature
	 */
	final private EActivityRelationship predicate;
	
	/**
	 * The activity playing the object role in the statement
	 */
	final private IActivitySpecification objectSpec;
	
	/**
	 * To construct a RelationshipStatement with the provided
	 * subject, predicate and object.
	 * The null object is not allowed for the parameters  subject, 
	 * predicate and object. An illegal Argument exception is consequently 
	 * thrown if a null id passed as argument
	 *  
	 * @param subject -- the subject of the statement 
	 * @param predicate -- the predicate of the statement
	 * @param object -- the object of the statement
	 */
	public RelationshipStatement(
						IActivitySpecification subjectSpec, 
						EActivityRelationship predicate, 
						IActivitySpecification objectSpec)	
	{
		//ASSERTION not null
		if( subjectSpec==null || 
			predicate==null ||
			objectSpec==null)
		{
			throw new IllegalArgumentException(
					"Constructor new RelationshipStatement does not allow"+
					"any null argument");
		}
		
		this.subjectSpec=subjectSpec;
		this.objectSpec=objectSpec;
		this.predicate=predicate;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IRelationshipStatement#getObject()
	 */
	public IActivitySpecification getObject()
	{
		return objectSpec;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IRelationshipStatement#getPredicate()
	 */
	public EActivityRelationship getPredicate()
	{
		return predicate;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IRelationshipStatement#getSubject()
	 */
	public IActivitySpecification getSubject()
	{
		return subjectSpec;
	}	
	
	@Override
	public boolean equals(Object obj)
	{
		if(obj==null)
		{
			return true;
		}
		else if(this==obj)
		{
			return true;
		}
		else if(obj instanceof IRelationshipStatement)
		{
			IRelationshipStatement stm=
				(IRelationshipStatement)obj;
			return 	subjectSpec.equals(stm.getSubject()) &&
					objectSpec.equals(stm.getObject()) &&
					predicate==stm.getPredicate();
		}
		else
		{
			return false;
		}
	}
	@Override
	public String toString()
	{
		StringBuffer buf= new StringBuffer(128);
		buf.append("ARel[");
		buf.append(subjectSpec);
		buf.append(',');
		buf.append(predicate);
		buf.append(',');
		buf.append(objectSpec);
		buf.append(']');
		return buf.toString();
	}
}
