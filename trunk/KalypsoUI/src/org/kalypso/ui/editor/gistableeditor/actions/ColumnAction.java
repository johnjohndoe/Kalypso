package org.kalypso.ui.editor.gistableeditor.actions;

import org.deegree.model.feature.Annotation;
import org.eclipse.jface.action.Action;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ogc.gml.table.command.SetColumnVisibleCommand;
import org.kalypso.util.command.ICommandTarget;

public final class ColumnAction extends Action
{
  private final ICommandTarget m_commandTarget;

  private final LayerTableViewer m_viewer;

  private final String m_propertyName;

  public ColumnAction( final ICommandTarget commandTarget, final LayerTableViewer viewer,
      final String propertyName, final Annotation annotation )
  {
    super( propertyName );
    if( annotation != null )
    {
      setText( annotation.getLabel() + " (" + propertyName + ")" );
      setDescription( annotation.getDescription() );
      setToolTipText( annotation.getTooltip() );
    }
    m_commandTarget = commandTarget;
    m_viewer = viewer;
    m_propertyName = propertyName;
    setChecked( viewer.hasColumn( propertyName ) );
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
    final SetColumnVisibleCommand setColumnVisibleCommand = new SetColumnVisibleCommand( m_viewer,
        m_propertyName, isChecked() );

    m_commandTarget.postCommand( setColumnVisibleCommand, null );
  }
}