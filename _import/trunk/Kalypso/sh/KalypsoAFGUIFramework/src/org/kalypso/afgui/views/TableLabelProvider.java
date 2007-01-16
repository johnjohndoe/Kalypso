/**
 * 
 */
package org.kalypso.afgui.views;

import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.viewers.IFontProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.model.EActivityAction;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IWorkflow;

/**
 * @author Patrice Congo
 *
 */
public class TableLabelProvider 
				implements 	ITableLabelProvider,
							IFontProvider
{
	private final EActivityAction[] ACTIONS=EActivityAction.values();
	private final String[] ACTIONS_STRINGS=makeActionString(ACTIONS);
	private final int ACTIONS_MAX_INDEX=ACTIONS.length;
	private ImageRegistry imageRegistry=
		KalypsoAFGUIFrameworkPlugin.getDefault().getImageRegistry();
	/**
	 * Knows the current root
	 */
	private WorkflowViewContentProvider cProvider;
	
	public TableLabelProvider(WorkflowViewContentProvider cProvider)
	{
		this.cProvider= cProvider;
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
	 */
	public Image getColumnImage(Object element, int columnIndex)
	{
		if(element instanceof IActivity)
		{
			if(columnIndex==0)
			{
				return null;
			}
			else
			{
				return null;
			}
		}
		else if(element==EActivityAction.class)
		{
			if(columnIndex<ACTIONS_MAX_INDEX)
			{
				return imageRegistry.get(ACTIONS_STRINGS[columnIndex]);
				
			}
			else
			{
				return null;
			}
		}
		else
		{
			return null;
		}
	}

	private String[] makeActionString(EActivityAction[] actions)
	{
		String[] as=new String[actions.length];
		for(int i=0;i<as.length;i++)
		{
			as[i]=actions[i].toString();
		}
		return as;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
	 */
	public String getColumnText(Object element, int columnIndex)
	{
		if(element instanceof IActivity)
		{
			if(columnIndex==0)
			{
				return ((IActivity)element).getName();
			}
			else
			{
				return "";//"IA"+columnIndex;
			}
		}
		else if(element==EActivityAction.class)
		{
			if(columnIndex<ACTIONS_MAX_INDEX)
			{
				return "";//ACTIONS[columnIndex].toString();
			}
			else
			{
				return "";
			}
		}
		else if(element instanceof IWorkflow)
		{
			return "Workflow";
		}
		else
		{
			return "";
		}
		
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
	 */
	public void addListener(ILabelProviderListener listener)
	{
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
	 */
	public void dispose()
	{

	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object, java.lang.String)
	 */
	public boolean isLabelProperty(Object element, String property)
	{
		return false;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
	 */
	public void removeListener(ILabelProviderListener listener)
	{
		// TODO Auto-generated method stub

	}

	static final String DEFAULT_FONT_NAME="Time new roman";//Display.getDefault().getActiveShell().getFont().getFontData()[0].getName();
	private static final Font ROOT_FONT= 
		new Font(Display.getDefault(),DEFAULT_FONT_NAME, 10, SWT.BOLD);

	private static final Font NON_ROOT_FONT=
		new Font(Display.getDefault(),DEFAULT_FONT_NAME,10, SWT.NORMAL);

public Font getFont(Object element)
{
	
	if(element==cProvider.getRoot())
	{
		return ROOT_FONT;
	}
	else
	{
		return NON_ROOT_FONT;
	}
}
}
