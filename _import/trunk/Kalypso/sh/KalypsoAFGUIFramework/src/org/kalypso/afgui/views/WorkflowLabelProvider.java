package org.kalypso.afgui.views;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IFontProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.workflow.Activity;
import org.kalypso.workflow.Phase;
import org.kalypso.workflow.Task;

public class WorkflowLabelProvider extends LabelProvider implements IFontProvider // , IColorProvider
{
  private final Image IMAGE_TASK;

  private final Image IMAGE_GROUP;

  private final Font FONT_PHASE;

  private final Font FONT_TASKGROUP;

  private final Font FONT_TASK;

  public WorkflowLabelProvider( final TreeViewer viewer )
  {
    final Display display = viewer.getControl().getDisplay();
    final ImageDescriptor taskImage = KalypsoAFGUIFrameworkPlugin.getImageDescriptor( "icons/nuvola_select/kig.png" );
    final ImageDescriptor groupImage = KalypsoAFGUIFrameworkPlugin.getImageDescriptor( "icons/nuvola_select/forward.png" );
    IMAGE_TASK = ImageDescriptor.createFromImageData( taskImage.getImageData().scaledTo( 16, 16 ) ).createImage();
    IMAGE_GROUP = ImageDescriptor.createFromImageData( groupImage.getImageData().scaledTo( 16, 16 ) ).createImage();
    FONT_PHASE = new Font( display, "Tahoma", 10, SWT.BOLD );
    FONT_TASKGROUP = new Font( display, "Tahoma", 10, SWT.NORMAL );
    FONT_TASK = new Font( display, "Tahoma", 9, SWT.NORMAL );
  }

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
   */
  @Override
  public Image getImage( final Object element )
  {
    if( element instanceof Task )
    {
      // TODO: get image from task
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
    if( element instanceof Activity )
    {
      return ((Activity) element).getName();
    }
    else
      return null;
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
    if( element instanceof Phase )
    {
      return FONT_PHASE;
    }
    else if( element instanceof Task )
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
