/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.views.contentprov;

import org.apache.log4j.Logger;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;

/**
 * @author congo
 *
 */
public class WorkflowDataLabelProvider implements ILabelProvider
{
	private static final Logger logger= 
				Logger.getLogger(WorkflowDataLabelProvider.class);
	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.ILabelProvider#getImage(java.lang.Object)
	 */
	public Image getImage(Object element)
	{
		try
		{
			return Kalypso1d2dProjectPlugin.getImageDescriptor(
					Kalypso1d2dProjectPlugin.KEY_ICON_SIM_MODEL);
		}
		catch(Throwable th)
		{
			logger.error("Error getting image", th);
			return null;
		}
	
	}

	/**
	 * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
	 */
	public String getText(Object element)
	{
		if(element instanceof IWorkflowData)
		{
			String name=((IWorkflowData)element).getName();
			if(name==null)
			{
				return element.toString();
			}
			else if (name.length()==0)
			{
				return element.toString();
			}
			else
			{
				return name;
			}				
		}
		else
		{
			return element.toString();
		}
	}

	/**
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
	 */
	public void addListener(ILabelProviderListener listener)
	{
		
	}

	/**
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
	 */
	public void dispose()
	{
		
	}

	/**
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object, java.lang.String)
	 */
	public boolean isLabelProperty(Object element, String property)
	{
		return false;
	}

	/**
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
	 */
	public void removeListener(ILabelProviderListener listener)
	{
		
	}

}
