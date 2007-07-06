package org.kalypso.afgui.views;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.ITableColorProvider;
import org.eclipse.jface.viewers.ITableFontProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;

import de.renew.workflow.base.Task;
import de.renew.workflow.base.TaskGroup;

public class WorkflowLabelProvider extends LabelProvider implements ITableLabelProvider, ITableFontProvider, ITableColorProvider
{

  private final Image IMAGE_TASK;

  private final Image IMAGE_GROUP;

  private final Font FONT_TASKGROUP;

  private final Font FONT_TASK;

  private final Font FONT_ACTIVE_TASK;

  private final Image IMAGE_UNAVAILABLE = KalypsoAFGUIFrameworkPlugin.getImageDescriptor( "icons/remove.gif" ).createImage();

  private final Image IMAGE_RUNNNING = KalypsoAFGUIFrameworkPlugin.getImageDescriptor( "icons/eclipse/running.gif" ).createImage();

  private final Image IMAGE_FINISHED = KalypsoAFGUIFrameworkPlugin.getImageDescriptor( "icons/eclipse/finished.gif" ).createImage();

  private final WorkflowControl m_workflowControl;

  public WorkflowLabelProvider( final WorkflowControl workflowControl )
  {
    m_workflowControl = workflowControl;
    final Display display = workflowControl.getTreeViewer().getControl().getDisplay();
    final ImageDescriptor taskImage = KalypsoAFGUIFrameworkPlugin.getImageDescriptor( "icons/nuvola_select/kig.png" );
    final ImageDescriptor groupImage = KalypsoAFGUIFrameworkPlugin.getImageDescriptor( "icons/nuvola_select/forward.png" );
    IMAGE_TASK = ImageDescriptor.createFromImageData( taskImage.getImageData().scaledTo( 16, 16 ) ).createImage();
    IMAGE_GROUP = ImageDescriptor.createFromImageData( groupImage.getImageData().scaledTo( 16, 16 ) ).createImage();
    final FontData[] fontData = JFaceResources.getFontRegistry().getFontData( JFaceResources.DIALOG_FONT );
    FONT_TASK = new Font( display, fontData );
    fontData[0].setStyle( SWT.BOLD );
    FONT_ACTIVE_TASK = new Font( display, fontData );
    fontData[0].setStyle( SWT.NORMAL );
    fontData[0].setHeight( fontData[0].getHeight() + 1 );
    FONT_TASKGROUP = new Font( display, fontData );
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
    if( element instanceof Task )
    {
      return ((Task) element).getName();
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
    IMAGE_GROUP.dispose();
    IMAGE_UNAVAILABLE.dispose();
    IMAGE_RUNNNING.dispose();
    IMAGE_FINISHED.dispose();
    FONT_TASK.dispose();
    FONT_TASKGROUP.dispose();
  }

  /**
   * @see org.eclipse.jface.viewers.IFontProvider#getFont(java.lang.Object)
   */
  public Font getFont( final Object element )
  {
    if( element instanceof TaskGroup )
    {
      return FONT_TASKGROUP;
    }
    else
    {
      if( m_workflowControl.getTaskExecutor().getActiveTask() == element )
        return FONT_ACTIVE_TASK;
      else
        return FONT_TASK;
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
   */
  public Image getColumnImage( final Object element, final int columnIndex )
  {
    if( element instanceof Task )
    {
      // Task task = (Task) element;
      if( columnIndex == 0 )
      {
        return getImage( element );
      }
      // else if( columnIndex == 1 )
      // {
      // switch( task.getState() )
      // {
      // case RUNNING:
      // return IMAGE_RUNNNING;
      // case FINISHED:
      // return IMAGE_FINISHED;
      // case UNAVAILABLE:
      // return IMAGE_UNAVAILABLE;
      // default:
      // return null;
      // }
      // }
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

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
   */
  public String getColumnText( final Object element, final int columnIndex )
  {
    if( columnIndex == 0 )
    {
      return getText( element );
    }
    else
    {
      return null;
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ITableFontProvider#getFont(java.lang.Object, int)
   */
  public Font getFont( final Object element, final int columnIndex )
  {
    return getFont( element );
  }

  /**
   * @see org.eclipse.jface.viewers.ITableColorProvider#getBackground(java.lang.Object, int)
   */
  public Color getBackground( final Object element, final int columnIndex )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITableColorProvider#getForeground(java.lang.Object, int)
   */
  public Color getForeground( final Object element, final int columnIndex )
  {
    return null;
  }
}
