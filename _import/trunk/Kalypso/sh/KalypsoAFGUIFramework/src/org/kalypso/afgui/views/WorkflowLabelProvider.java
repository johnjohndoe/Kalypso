package org.kalypso.afgui.views;

import org.eclipse.jface.viewers.IFontProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.ITask;

public class WorkflowLabelProvider extends LabelProvider implements IFontProvider // , IColorProvider
{
  private final Image IMAGE_TASK = KalypsoAFGUIFrameworkPlugin.getImageDescriptor( "icons/nuvola_select/kig.png" ).createImage();

  private final Image IMAGE_GROUP = KalypsoAFGUIFrameworkPlugin.getImageDescriptor( "icons/nuvola_select/forward.png" ).createImage();

  private final Font FONT_PHASE;

  private final Font FONT_TASKGROUP;

  private final Font FONT_TASK;

  public WorkflowLabelProvider( final TreeViewer viewer )
  {
    final Display display = viewer.getControl().getDisplay();
    FONT_PHASE = new Font( display, "Tahoma", 12, SWT.NORMAL );
    FONT_TASKGROUP = new Font( display, "Tahoma", 11, SWT.NORMAL );
    FONT_TASK = new Font( display, "Tahoma", 10, SWT.NORMAL );
  }

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
   */
  @Override
  public Image getImage( final Object element )
  {
    if( element instanceof ITask )
    {
      return IMAGE_TASK;
    }
    else
    {
      return IMAGE_GROUP;
    }
  }

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
   */
  @Override
  public String getText( final Object element )
  {
    return super.getText( element );
  }

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#dispose()
   */
  @Override
  public void dispose( )
  {
    IMAGE_TASK.dispose();
  }

  /**
   * @see org.eclipse.jface.viewers.IFontProvider#getFont(java.lang.Object)
   */
  public Font getFont( final Object element )
  {
    if( element instanceof IPhase )
    {
      return FONT_PHASE;
    }
    else if( element instanceof ITask )
    {
      return FONT_TASK;
    }
    else
    {
      return FONT_TASKGROUP;
    }
  }

  // /**
  // * @see org.eclipse.jface.viewers.IColorProvider#getBackground(java.lang.Object)
  // */
  // public Color getBackground( final Object element )
  // {
  // // TODO Auto-generated method stub
  // return null;
  // }
  //
  // /**
  // * @see org.eclipse.jface.viewers.IColorProvider#getForeground(java.lang.Object)
  // */
  // public Color getForeground( final Object element )
  // {
  // // TODO Auto-generated method stub
  // return null;
  // }
}
