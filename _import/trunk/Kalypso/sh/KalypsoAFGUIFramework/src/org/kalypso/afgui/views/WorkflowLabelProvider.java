package org.kalypso.afgui.views;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;

import de.renew.workflow.base.Task;
import de.renew.workflow.base.TaskGroup;

/**
 * @author Stefan Kurzbach
 * 
 */
public class WorkflowLabelProvider extends ColumnLabelProvider
{
  private final Image IMAGE_TASK;

  private final Image IMAGE_GROUP;

  private final Font FONT_TASKGROUP;

  private final Font FONT_TASK;

  private final Font FONT_ACTIVE_TASK;

  private final WorkflowControl m_workflowControl;

  public WorkflowLabelProvider( final WorkflowControl workflowControl )
  {
    m_workflowControl = workflowControl;
    final Display display = workflowControl.getTreeViewer().getControl().getDisplay();
    final ImageDescriptor taskImage = KalypsoAFGUIFrameworkPlugin.getImageDescriptor( "icons/nuvola_select/kig.png" );  //$NON-NLS-1$
    final ImageDescriptor groupImage = KalypsoAFGUIFrameworkPlugin.getImageDescriptor( "icons/nuvola_select/forward.png" );  //$NON-NLS-1$
    IMAGE_TASK = ImageDescriptor.createFromImageData( taskImage.getImageData().scaledTo( 16, 16 ) ).createImage();
    IMAGE_GROUP = ImageDescriptor.createFromImageData( groupImage.getImageData().scaledTo( 16, 16 ) ).createImage();
    final FontData[] fontData = JFaceResources.getFontRegistry().getFontData( JFaceResources.DIALOG_FONT );
    final String dialogFontName = fontData[0].getName();
    final int dialogFontHeight = fontData[0].getHeight();
    FONT_TASK = new Font( display, fontData );
    final FontData boldFont = new FontData( dialogFontName, dialogFontHeight, SWT.BOLD );
    FONT_ACTIVE_TASK = new Font( display, boldFont );
    final FontData bigFont = new FontData( dialogFontName, dialogFontHeight + 1, SWT.NORMAL );
    FONT_TASKGROUP = new Font( display, bigFont );
  }

  /**
   * @see org.eclipse.jface.viewers.ColumnLabelProvider#getImage(java.lang.Object)
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
   * @see org.eclipse.jface.viewers.ColumnLabelProvider#getText(java.lang.Object)
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
   * @see org.eclipse.jface.viewers.BaseLabelProvider#dispose()
   */
  @Override
  public void dispose( )
  {
    IMAGE_TASK.dispose();
    IMAGE_GROUP.dispose();
    FONT_TASK.dispose();
    FONT_TASKGROUP.dispose();
    super.dispose();
  }

  /**
   * @see org.eclipse.jface.viewers.ColumnLabelProvider#getFont(java.lang.Object)
   */
  @Override
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
   * @see org.eclipse.jface.viewers.CellLabelProvider#getToolTipText(java.lang.Object)
   */
  @Override
  public String getToolTipText( final Object element )
  {
    if( element instanceof Task )
    {
      Task task = (Task) element;
      return task.getHelp().getValue();
    }
    else
    {
      return null;
    }
  }
}
