package org.kalypso.ogc.gml.mapactions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;


/**
 * @author belger
 */
public abstract class AbstractCommandAction extends Action 
{
  private final MapPanel m_mapPanel;
  private final ICommandTarget m_commandTarget;

  public AbstractCommandAction( final ICommandTarget commandTarget, final MapPanel mapPanel, final String text, final ImageDescriptor imageDescriptor, final String tooltiptext )
  {
    super( text, AS_PUSH_BUTTON );
   
    setToolTipText( tooltiptext );
    setImageDescriptor( imageDescriptor );
    
    m_mapPanel = mapPanel;
    m_commandTarget = commandTarget;
  }
  
  protected abstract ICommand runInternal();

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public final void run()
  {
    postCommand( runInternal(), null );
  }

  
  protected final MapPanel getMapPanel()
  {
    return m_mapPanel;
  }
  
  protected final void postCommand( final ICommand command, final Runnable runAfter )
  {
    m_commandTarget.postCommand(command, runAfter);
  }
}