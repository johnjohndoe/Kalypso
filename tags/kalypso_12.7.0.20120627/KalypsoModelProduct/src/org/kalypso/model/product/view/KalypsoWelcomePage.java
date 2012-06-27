/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.product.view;

import java.awt.Point;
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.help.IWorkbenchHelpSystem;
import org.eclipse.ui.part.IntroPart;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.swt.canvas.DefaultContentArea;
import org.kalypso.contribs.eclipse.swt.canvas.ImageCanvas2;
import org.kalypso.model.product.KalypsoModelProductPlugin;
import org.kalypso.model.product.i18n.Messages;
import org.kalypso.module.IKalypsoModule;
import org.kalypso.module.welcome.IKalypsoWelcomePage;
import org.kalypso.module.welcome.ModulePageComposite;
import org.kalypso.module.welcome.WelcomePageComposite;

/**
 * @author Dirk Kuch
 */
public class KalypsoWelcomePage extends IntroPart implements IKalypsoWelcomePage
{
  public static final String PART_ID = "org.kalypso.model.product.introID"; //$NON-NLS-1$

  private static final String KALYPSO_WEB_PAGE = "http://kalypso.sourceforge.net/"; //$NON-NLS-1$

  private static final Image IMG_BACKGROUND = new Image( null, KalypsoWelcomePage.class.getResourceAsStream( "images/background.gif" ) ); //$NON-NLS-1$

  protected static final Image IMG_FOOTER = new Image( null, KalypsoWelcomePage.class.getResourceAsStream( "images/footer.gif" ) ); //$NON-NLS-1$

  protected static final Image IMG_HELP = new Image( null, KalypsoWelcomePage.class.getResourceAsStream( "images/help_browser.gif" ) ); //$NON-NLS-1$

  protected static final Image IMG_HOME = new Image( null, KalypsoWelcomePage.class.getResourceAsStream( "images/back.gif" ) ); //$NON-NLS-1$

  Composite m_contentArea;

  private Composite m_contentClient;

  private static IKalypsoModule m_selectedModule = null;

  protected ScrolledForm m_form;

  /**
   * @see org.eclipse.ui.part.IntroPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    final FormToolkit toolkit = KalypsoModelProductPlugin.getFormToolkit();

    m_form = toolkit.createScrolledForm( parent );
    final Composite body = m_form.getBody();

    body.setBackgroundImage( IMG_BACKGROUND );
    body.setBackgroundMode( SWT.INHERIT_FORCE );
    body.setLayout( new GridLayout() );
    final GridData layoutData = new GridData( GridData.FILL, GridData.FILL, true, true );
    layoutData.heightHint = 800;
    body.setLayoutData( layoutData );

    m_contentArea = new Composite( body, SWT.NULL );
    m_contentArea.setLayout( new GridLayout() );
    m_contentArea.setLayoutData( layoutData );

    updateControl();

    m_form.reflow( false );
  }

  public void updateControl( )
  {
    if( m_contentArea.isDisposed() )
      return;

    if( m_contentClient != null )
    {
      if( !m_contentClient.isDisposed() )
      {
        m_contentClient.dispose();
      }

      m_contentClient = null;
    }

    if( m_selectedModule == null )
    {
      m_contentClient = new WelcomePageComposite( m_contentArea, SWT.NULL, this );
    }
    else
    {
      final FormToolkit toolkit = KalypsoModelProductPlugin.getFormToolkit();
      m_contentClient = new ModulePageComposite( m_selectedModule, toolkit, m_contentArea, SWT.NULL );
    }

    m_contentClient.addPaintListener( new PaintListener()
    {
      private org.eclipse.swt.graphics.Point m_size;

      @Override
      public void paintControl( final PaintEvent e )
      {
        final org.eclipse.swt.graphics.Point size = m_contentArea.getSize();
        if( size == null )
          return;

        if( m_size == null )
        {
          m_size = size;
          m_form.reflow( false );
        }
        else if( !m_size.equals( size ) )
        {
          m_size = size;
          m_form.reflow( false );
        }
      }
    } );

    /* footer */
    final ImageCanvas2 footer = new ImageCanvas2( m_contentClient, SWT.NO_REDRAW_RESIZE );
    final Rectangle footerBounds = IMG_FOOTER.getBounds();

    final GridData footerData = new GridData( GridData.FILL, GridData.FILL, true, false, 2, 0 );
    footerData.heightHint = footerData.minimumHeight = footerBounds.height;
    footer.setLayoutData( footerData );

    final DefaultContentArea footerHelpContent = new DefaultContentArea()
    {

      @Override
      public Point getContentAreaAnchorPoint( )
      {
        return new Point( 5, footerBounds.height - IMG_HELP.getBounds().height - 10 );
      }
    };

    footerHelpContent.setImage( IMG_HELP );
    footerHelpContent.setTooltip( Messages.getString( "org.kalypso.model.product.view.KalypsoWelcomePage.4" ) ); //$NON-NLS-1$
    footerHelpContent.setMouseListener( new MouseAdapter()
    {
      /**
       * @see org.eclipse.swt.events.MouseAdapter#mouseUp(org.eclipse.swt.events.MouseEvent)
       */
      @Override
      public void mouseUp( final MouseEvent e )
      {
        final IWorkbenchHelpSystem helpSystem = PlatformUI.getWorkbench().getHelpSystem();
// helpSystem.displayHelp( "org.kalypso.KalypsoManualRisk.KalypsoRisikoDatenladen" );
        helpSystem.displayHelp();
      }
    } );

// PlatformUI.getWorkbench().getHelpSystem().setHelp( footer, "org.kalypso.KalypsoManualRisk.KalypsoRisikoDatenladen" );
// PlatformUI.getWorkbench().getHelpSystem().setHelp( m_contentArea,
// "org.kalypso.KalypsoManualRisk.KalypsoRisikoDatenladen" );

    footer.addContentArea( footerHelpContent );

    if( m_selectedModule != null )
    {
      final DefaultContentArea footerBackContent = new DefaultContentArea()
      {
        @Override
        public Point getContentAreaAnchorPoint( )
        {
          final Rectangle imgBounds = IMG_HELP.getBounds();
          return new Point( imgBounds.width + 20, footerBounds.height - IMG_HELP.getBounds().height - 10 );
        }
      };

      footerBackContent.setImage( IMG_HOME );
      footerBackContent.setTooltip( Messages.getString( "org.kalypso.model.product.view.KalypsoWelcomePage.5" ) ); //$NON-NLS-1$
      footerBackContent.setMouseListener( new MouseAdapter()
      {
        /**
         * @see org.eclipse.swt.events.MouseAdapter#mouseUp(org.eclipse.swt.events.MouseEvent)
         */
        @Override
        public void mouseUp( final MouseEvent e )
        {
          setSelectedModule( null );
        }
      } );

      footer.addContentArea( footerBackContent );
    }

    final DefaultContentArea footerLogo = new DefaultContentArea()
    {
      @Override
      public Point getContentAreaAnchorPoint( )
      {
        return new Point( footer.getBounds().width - footerBounds.width, 0 );
      }
    };

    footerLogo.setImage( IMG_FOOTER );
    final String footerLogoTooltip = Messages.getString( "org.kalypso.model.product.view.KalypsoWelcomePage.6" ); //$NON-NLS-1$
    footerLogo.setTooltip( footerLogoTooltip ); //$NON-NLS-1$
    footerLogo.setMouseListener( new MouseAdapter()
    {
      /**
       * @see org.eclipse.swt.events.MouseAdapter#mouseUp(org.eclipse.swt.events.MouseEvent)
       */
      @Override
      public void mouseUp( final MouseEvent e )
      {
        try
        {
          final IWorkbenchBrowserSupport browserSupport = PlatformUI.getWorkbench().getBrowserSupport();
          final IWebBrowser externalBrowser = browserSupport.getExternalBrowser();
          externalBrowser.openURL( new URL( KALYPSO_WEB_PAGE ) );
        }
        catch( final PartInitException ex )
        {
          final String message = String.format( Messages.getString("KalypsoWelcomePage.1"), KALYPSO_WEB_PAGE ); //$NON-NLS-1$
          MessageDialog.openError( e.widget.getDisplay().getActiveShell(), footerLogoTooltip, message );
          KalypsoModelProductPlugin.getDefault().getLog().log( ex.getStatus() );
        }
        catch( final MalformedURLException ex )
        {
          final String message = String.format( Messages.getString("KalypsoWelcomePage.0"), KALYPSO_WEB_PAGE ); //$NON-NLS-1$
          MessageDialog.openError( e.widget.getDisplay().getActiveShell(), footerLogoTooltip, message );
        }
      }
    } );

    footer.addContentArea( footerLogo );

    m_contentClient.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    m_contentArea.layout();
  }

  /**
   * @see org.eclipse.ui.part.IntroPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
  }

  /**
   * @see org.eclipse.ui.intro.IIntroPart#standbyStateChanged(boolean)
   */
  @Override
  public void standbyStateChanged( final boolean standby )
  {
  }

  /**
   * @see org.kalypso.project.database.client.extension.pages.welcome.IKalypsoWelcomePage#setSelectedModule(org.kalypso.project.database.client.extension.IKalypsoModule)
   */
  @Override
  public void setSelectedModule( final IKalypsoModule module )
  {
    m_selectedModule = module;

    new UIJob( "" ) //$NON-NLS-1$
    {

      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        updateControl();
        return Status.OK_STATUS;
      }
    }.schedule();
  }

}
