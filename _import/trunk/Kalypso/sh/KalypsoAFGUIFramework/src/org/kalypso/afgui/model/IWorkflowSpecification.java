/**
 * 
 */
package org.kalypso.afgui.model;

import java.util.List;


/**
 * The Interface to implement by classes that provide the workflow
 * specification service. This means the implementing class knows about:
 * <ul>
 * 	<li/>The Task and activities in the workflow
 *  <li/>The relationship between the Task and activities
 *  <li/>The data model associated to the tasks ode activities
 *  <li/>Also the gui Elements associated to the tasks and activities.
 *</ul>
 * @author Patrice Congo
 *
 */
public interface IWorkflowSpecification
{

	/**
	 * To get the specification for the Activity with the given name.
	 * @param name -- the name of activity which specification is being retrieved
	 * @return the specification for the named activity or null if no activity 
	 * 			with the specified name exists 
	 */
	public IActivitySpecification getActivitySpecificationByName(String name);
	
	/**
	 * To retrieve the children activity of a given activity.
	 * The childen activity are linked to a parent activities through a part-of
	 * relationship.
	 * This methods never returns null. if there is no linked Activity found 
	 * an empty list is return.
	 * @return a list containing the resolved specification 
	 * 
	 */
	public List<IActivitySpecification> getLinkedActivitySpecs(
											IActivitySpecification spec,
											EActivityRelationship aRelationship);
	
	/**
	 * Select a relationship the set of the relationship a workflow contains.
	 * The selection is specify by the triple (subjectSpec,predicate,objectSpec).
	 * The null is in this context as wild-card for object and subject sprcification.
	 * {@link EActivityRelationship#ANY} is the wildcard for predicate.
	 * 
	 * That is (null, predicate1,objectSpec1) will select relationships in the 
	 * workflow with matching predicate and object spec.
	 * 
	 * @param subject -- the expected subject in the relationship null is used as wildcard
	 * @param predicate -- the expected predicate of the relation ship 
	 * @param object -- the expected object specification in the relationship
	 * 
	 * @return a list of relationships matching the provided tripple
	 */
	public List<IRelationshipStatement> selectRelationships(
											IActivitySpecification subjectSpec,
											EActivityRelationship predicate,
											IActivitySpecification objectSpec);
	/**
	 * Return a list of all root activity in the workspace.
	 * An Activity is c
	 * @return
	 */
	public List<IActivitySpecification> getRootActivitySpecs();
	
}
