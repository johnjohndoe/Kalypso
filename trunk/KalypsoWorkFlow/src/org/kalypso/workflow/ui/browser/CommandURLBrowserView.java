package org.kalypso.workflow.ui.browser;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.actions.ActionFactory;
import org.kalypso.contribs.eclipse.ui.browser.AbstractBrowserView;
import org.kalypso.workflow.WorkflowContext;
import org.kalypso.workflow.ui.KalypsoWorkFlowPlugin;

/**
 * BrowserView that can handle and excecute CommandURLs via CommandLocationListener<br>
 * 
 * @author kuepfer
 */
public class CommandURLBrowserView extends AbstractBrowserView
{
  public static final String WEB_BROWSER_VIEW_ID = "org.kalypso.contribs.ui.browser.commandable.CommandURLBrowserView"; //$NON-NLS-1$

  protected final CommandLocationListener m_listener;

  private WorkflowContext m_workflowContext;

  public CommandURLBrowserView( )
  {
    m_workflowContext = KalypsoWorkFlowPlugin.getDefault().getDefaultWorkflowContext();
    m_listener = new CommandLocationListener( m_workflowContext, this );
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.browser.AbstractBrowserView#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( Composite parent )
  {
    super.createPartControl( parent );
    configuerActionBar();
    updateNavigationActionsState();
    addLocationListener( m_listener );
  }

  protected Browser getBrowser( )
  {
    return m_viewer.getBrowser();
  }

  private void configuerActionBar( )
  {
    final IActionBars actionBars = getActionBars();
    final IToolBarManager toolBarManager = actionBars.getToolBarManager();
    actionBars.setGlobalActionHandler( ActionFactory.REFRESH.getId(), m_refresh );
    actionBars.setGlobalActionHandler( ActionFactory.FORWARD.getId(), m_forwardAction );
    actionBars.setGlobalActionHandler( ActionFactory.BACK.getId(), m_backAction );
    toolBarManager.add( m_refresh );
    toolBarManager.add( m_backAction );
    toolBarManager.add( m_forwardAction );
    toolBarManager.update( true );
    actionBars.updateActionBars();
  }

  protected Action m_backAction = new Action()
  {

    {
      setToolTipText( "Zur�ck" );
      setImageDescriptor( ImageDescriptor.createFromURL( getClass().getResource( "icons/backward_nav.gif" ) ) ); //$NON-NLS-1$
      setDisabledImageDescriptor( ImageDescriptor.createFromURL( getClass().getResource( "icons/backward_nav_gray.gif" ) ) ); //$NON-NLS-1$
    }

    @Override
    public void run( )
    {
      navigateBack();
    }
  };

  protected Action m_forwardAction = new Action()
  {

    {
      setToolTipText( "Vorw�rts" );
      setImageDescriptor( ImageDescriptor.createFromURL( getClass().getResource( "icons/forward_nav.gif" ) ) ); //$NON-NLS-1$
      setDisabledImageDescriptor( ImageDescriptor.createFromURL( getClass().getResource( "icons/forward_nav_gray.gif" ) ) ); //$NON-NLS-1$
    }

    @Override
    public void run( )
    {
      navigateForward();
    }
  };

  protected Action m_refresh = new Action()
  {

    {
      setToolTipText( "Refresh" );
      setImageDescriptor( ImageDescriptor.createFromURL( getClass().getResource( "icons/refresh.gif" ) ) );
    }

    @Override
    public void run( )
    {
      m_viewer.refresh();
    }
  };

  protected void updateNavigationActionsState( )
  {

    // in static html intro, use browser history.
    m_forwardAction.setEnabled( m_viewer.isForwardEnabled() );
    m_backAction.setEnabled( m_viewer.isBackEnabled() );
  }

  protected void navigateBack( )
  {
    m_viewer.back();
    try
    {
      m_workflowContext.setContextActionURL( new URL( m_viewer.getURL() ) );
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
    }
    updateNavigationActionsState();
  }

  protected void navigateForward( )
  {
    m_viewer.forward();
    try
    {
      m_workflowContext.setContextActionURL( new URL( m_viewer.getURL() ) );
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
    }
    updateNavigationActionsState();
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.browser.AbstractBrowserView#setURL(java.lang.String)
   */
  @Override
  public void setURL( String url )
  {
    super.setURL( url );
    updateNavigationActionsState();

  }
}
