/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.views.contentprov;

import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.model.EWorkflowProperty;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.kalypso1d2d.pjt.ActiveWorkContext;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;

/**
 * Content provider for the simulation model based data view
 * 
 * @author Patrice Congo
 */
public class SimModelBasedContentProvider implements ITreeContentProvider
{
	final static private Logger logger=
				Logger.getLogger(SimModelBasedContentProvider.class);
	
	final static public Object[] EMPTY_ARRAY={};
	
	
	
	//private IWorkflowDB workflowDB;
	
	
	public Object[] getChildren(Object parentElement)
	{
		if(parentElement instanceof IWorkflowData)
		{
			IWorkflowData workflowData= (IWorkflowData)parentElement;
			List<IWorkflowData> list=
					workflowData.getLinkedWorkflowData(
									EWorkflowProperty.IS_DERIVED_FROM);
			return list.toArray();
		}
		else
		{
			return EMPTY_ARRAY;
		}
	}

	public Object getParent(Object element)
	{
		return null;
	}

	public boolean hasChildren(Object element)
	{
		if(element instanceof IWorkflowData)
		{
			IWorkflowData workflowData= (IWorkflowData)element;
			return workflowData.hasLinkedWorkflowData(
									EWorkflowProperty.IS_DERIVED_FROM);
			
		}
		else
		{
			return false;
		}
	}

	public Object[] getElements(Object inputElement)
	{
		logger.info("getting root elements:"+inputElement);
		if(inputElement instanceof ActiveWorkContext)
		{
			ActiveWorkContext workContext= (ActiveWorkContext)inputElement;
			IWorkflowDB workflowDB=workContext.getWorkflowDB();
			if(workflowDB==null)
			{
				logger.warn("Workflow DB is null");
				return EMPTY_ARRAY;
			}
			else
			{
				List<IWorkflowData> data=
					workflowDB.getRootWorkflowDataByType(
								Kalypso1D2DSchemaConstants.SIMULATION_MODEL1D2D.toString());
				return data.toArray();
			}
		}
		else
		{
			logger.warn("Not supportetd root:"+inputElement);
			return EMPTY_ARRAY;
		}
	}
	
	
	public void dispose()
	{
					
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput)
	{
		
	}
	
}