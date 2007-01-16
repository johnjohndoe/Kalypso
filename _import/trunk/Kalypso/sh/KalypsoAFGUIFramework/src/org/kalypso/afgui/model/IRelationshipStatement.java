package org.kalypso.afgui.model;

public interface IRelationshipStatement
{

	/**
	 * To get the activity which is playing the role of object in the 
	 * statement 
	 * @return the object of the statement.
	 */
	public IActivitySpecification getObject();

	/**
	 * To get the predicate of the statement.
	 * @return the predicate of the statement
	 */
	public EActivityRelationship getPredicate();

	/**
	 * To the activity which is playing the rol of subject in the statment 
	 * @return the subject of the statment
	 */
	public IActivitySpecification getSubject();
	
	

}