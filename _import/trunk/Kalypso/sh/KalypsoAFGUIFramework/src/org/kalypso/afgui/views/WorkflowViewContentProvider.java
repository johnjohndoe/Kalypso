/**
 * 
 */
package org.kalypso.afgui.views;



import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IWorkflow;

public class WorkflowViewContentProvider 
			implements ITreeContentProvider
{
	
	private IActivity EMPTY[]={};
	private IWorkflow workflow;
	private Object root;
	
	public WorkflowViewContentProvider(IWorkflow workflow)
	{
		if(workflow==null)
		{
			throw new IllegalArgumentException(
					"Argument workflow must not be null");
		}
		this.workflow=workflow;
	}
	
	public Object[] getChildren(Object parentElement)
	{
//		if(parentElement instanceof IActivity)
//		{
////			IActivity rootActivity=null;
////			if(root instanceof IActivity[])
////			{
////				rootActivity=((IActivity[])root)[0];
////			}
//			if(parentElement.equals(root/*Activity*/))
//			{
//				List<IActivity> children=
//					workflow.getChildrenActivities(
//								(IActivity)parentElement, 
//								EActivityRelationship.HAS_A);
//				return children.toArray();
//			}
//			else
//			{
//				
//				return EMPTY;
//			}
//		}
//		else if(parentElement instanceof IWorkflow)
//		{
//			return workflow.getRootActivities().toArray();
//		}
//		else
//		{
//			return EMPTY;
//		}
		return null;
	}

	public Object getParent(Object element)
	{
		if(element instanceof IActivity)
		{
//			List<IActivity> parents=
//				workflow.getChildrenActivities(
//							(IActivity)element, 
//							EActivityRelationship.HAS_A);
//			int size=parents.size();
//			if(size==1)
//			{
//				return parents.get(0);
//			}
//			else if(size >1)
//			{
//				//TODO log
//				return parents.get(0);
//			}
//			else
//			{
//				//empty list
//				return null;
//			}
			return null;
		}
		else
		{
			return EMPTY;
		}
	}

	public boolean hasChildren(Object element)
	{
		if(element==root)
		{
//			if(element instanceof IActivity)
//			{
//				List<IActivity> children= 
//					workflow.getChildrenActivities(
//								(IActivity)element, 
//								EActivityRelationship.HAS_A);
//				return !children.isEmpty();
//				//return false;//true;
//			}
//			else if(element instanceof IWorkflow)
//			{
//				return !((IWorkflow)element).getRootActivities().isEmpty();
//			}
//			else
//			{
//				return false;
//			}
			return false;
		}
		else
		{
			return false;
		}
		
	}

	public Object[] getElements(Object inputElement)
	{
		//root=inputElement;
		if(inputElement instanceof IWorkflow)
		{
			this.workflow=(IWorkflow)inputElement;
			root=inputElement;
			return workflow.getPhases().toArray();//workflow.getRootActivities().toArray();
			
		}
		else if(inputElement instanceof IWorkflow[])
		{
			this.root=((IWorkflow[])inputElement)[0];
//			return ((IWorkflow[])inputElement)[0].getRootActivities().toArray();
			return ((IWorkflow[])inputElement)[0].getPhases().toArray();
			//return (Object[])inputElement;
		}
		else if(inputElement instanceof IActivity[])
		{
			this.root=((IActivity[])inputElement)[0];
			return (Object[])inputElement;
//			return workflow.getChildrenActivities(
//							(IActivity)inputElement, 
//							EActivityRelationship.HAS_A).toArray();
		}
		else
		{
			return EMPTY;
		}
	}

	
	public void dispose()
	{
		workflow=null;
		
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput)
	{
		
		
	}
	
	public Object getRoot()
	{
		return root;
	}
}