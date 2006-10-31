/**
 * 
 */
package org.kalypso.afgui.views;

import java.util.Map;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;

/**
 * @author congo
 *
 */
public class ModelDataView extends ViewPart
{
	static public final String ID="org.kalypso.afgui.views.ModelDataView";
	static public final String MODEL_STATIC="Static Model";
	static public final String MODEL_OP="Operational Model";
	static public final String MODEL_CNTL="Simulation Control Model";
	static public final String MODEL_RES="Result model";
	static public final Object[] EMPTY={};
	private TreeViewer tv;
	private ITreeContentProvider cProvider=
		new ITreeContentProvider()
	{
		
		
		public Object[] getChildren(Object parentElement)
		{
			if(parentElement==MODEL_STATIC)
			{
				return new String[]{"Static model 1"," static model 2"," Static model 3"};
			}
			else if(parentElement==MODEL_OP)
			{
				return new String[]{"Op model 1","operational model 2"};
			}
			else if(parentElement==MODEL_CNTL)
			{
				return new String[]{"Control model 1"};
			}
			else
			{
				return EMPTY;
			}
		}

		public Object getParent(Object element)
		{
			return null;
		}

		public boolean hasChildren(Object element)
		{
			return getChildren(element).length>0;
		}

		public Object[] getElements(Object inputElement)
		{			
			return new String[]{MODEL_STATIC, MODEL_OP, MODEL_CNTL,MODEL_RES};
		}

		public void dispose()
		{
			
		}

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput)
		{
			
		}
		
	};

	private ITreeContentProvider simModelProvider=
		new ITreeContentProvider()
	{
		
		
		public Object[] getChildren(Object parentElement)
		{
			if(parentElement==MODEL_STATIC)
			{
				return new String[]{"Static model 1"," static model 2"," Static model 3"};
			}
			else if(parentElement==MODEL_OP)
			{
				return new String[]{"Op model 1","operational model 2"};
			}
			else if(parentElement==MODEL_CNTL)
			{
				return new String[]{"Control model 1"};
			}
			else
			{
				return EMPTY;
			}
		}

		public Object getParent(Object element)
		{
			return null;
		}

		public boolean hasChildren(Object element)
		{
			return getChildren(element).length>0;
		}

		public Object[] getElements(Object inputElement)
		{			
			return new String[]{MODEL_STATIC, MODEL_OP, MODEL_CNTL,MODEL_RES};
		}

		public void dispose()
		{
			
		}

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput)
		{
			
		}
		
	};
	
	
	/* (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createPartControl(Composite parent)
	{
		tv= new TreeViewer(parent,SWT.FILL);
		tv.setContentProvider(cProvider);
		tv.setInput("");
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
	 */
	@Override
	public void setFocus()
	{
		// TODO Auto-generated method stub

	}

}
