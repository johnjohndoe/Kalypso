/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.help.IWorkbenchHelpSystem;
import org.eclipse.ui.part.IntroPart;
import org.kalypso.afgui.extension.IKalypsoModuleEnteringPageHandler;
import org.kalypso.afgui.extension.IKalypsoModulePageHandler;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.swt.canvas.DefaultContentArea;
import org.kalypso.contribs.eclipse.swt.canvas.ImageCanvas2;
import org.kalypso.model.product.KalypsoModelProductPlugin;
import org.kalypso.model.product.ui.ModuleEnteringPageComposite;
import org.kalypso.model.product.ui.WelcomePageIndexComposite;

/**
 * @author Dirk Kuch
 */
public class KalypsoWelcomePage extends IntroPart implements IKalypsoModulePageHandler
{
  private static final Image IMG_BACKGROUND = new Image( null, KalypsoWelcomePage.class.getResourceAsStream( "images/background.gif" ) );

  protected static final Image IMG_FOOTER = new Image( null, KalypsoWelcomePage.class.getResourceAsStream( "images/footer.gif" ) );

  protected static final Image IMG_HELP = new Image( null, KalypsoWelcomePage.class.getResourceAsStream( "images/help_browser.gif" ) );

  protected static final Image IMG_HOME = new Image( null, KalypsoWelcomePage.class.getResourceAsStream( "images/back.gif" ) );

  private Composite m_contentArea;

  private Composite m_contentClient;

  private IKalypsoModuleEnteringPageHandler m_page;

  /**
   * @see org.eclipse.ui.part.IntroPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    final FormToolkit toolkit = KalypsoModelProductPlugin.getFormToolkit();

    final ScrolledForm form = toolkit.createScrolledForm( parent );
    final Composite body = form.getBody();
    body.setBackgroundImage( IMG_BACKGROUND );
    body.setBackgroundMode( SWT.INHERIT_FORCE );
    body.setLayout( new GridLayout() );
    final GridData layoutData = new GridData( GridData.FILL, GridData.FILL, true, true );
    layoutData.heightHint = 800;
    body.setLayoutData( layoutData );

    m_contentArea = new Composite( body, SWT.NULL );
    m_contentArea.setLayout( new GridLayout() );
    m_contentArea.setLayoutData( layoutData );

    update();

    form.reflow( false );
  }

  public void update( )
  {
    if( m_contentArea.isDisposed() )
    {
      return;
    }

    if( m_contentClient != null )
    {
      if( !m_contentClient.isDisposed() )
      {
        m_contentClient.dispose();
      }

      m_contentClient = null;
    }

    if( m_page == null )
    {
      m_contentClient = new WelcomePageIndexComposite( m_contentArea, SWT.NULL, this );
    }
    else
    {
      m_contentClient = new ModuleEnteringPageComposite( m_contentArea, SWT.NULL, m_page, this );
    }

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
    footerHelpContent.setMouseListener( new MouseAdapter()
    {
      /**
       * @see org.eclipse.swt.events.MouseAdapter#mouseUp(org.eclipse.swt.events.MouseEvent)
       */
      @Override
      public void mouseUp( final MouseEvent e )
      {
        final IWorkbenchHelpSystem helpSystem = PlatformUI.getWorkbench().getHelpSystem();
        helpSystem.displayHelp();
      }
    }, "Hilfe" );

    footer.addContentArea( footerHelpContent );

    if( m_page != null )
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
      footerBackContent.setMouseListener( new MouseAdapter()
      {
        /**
         * @see org.eclipse.swt.events.MouseAdapter#mouseUp(org.eclipse.swt.events.MouseEvent)
         */
        @Override
        public void mouseUp( final MouseEvent e )
        {
          setPage( null );
          update();
        }
      }, "Zurück zur Startseite" );

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
    footerLogo.setMouseListener( new MouseAdapter()
    {
      /**
       * @see org.eclipse.swt.events.MouseAdapter#mouseUp(org.eclipse.swt.events.MouseEvent)
       */
      @Override
      public void mouseUp( final MouseEvent e )
      {
        // TODO: platform dependent; try to find other solution
        final String command = "cmd /C start http://kalypso.sourceforge.net/"; //$NON-NLS-1$

        try
        {
          Runtime.getRuntime().exec( command );
        }
        catch( final Exception e1 )
        {
          KalypsoModelProductPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e1 ) );
        }

      }
    }, "Weitere Informationen zu Kalypso" );

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
  public void standbyStateChanged( final boolean standby )
  {
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.extension.IKalypsoModulePageHandler#setPage(org.kalypso.kalypsosimulationmodel.extension.IKalypsoModuleEnteringPageHandler)
   */
  @Override
  public void setPage( final IKalypsoModuleEnteringPageHandler page )
  {
    m_page = page;
  }

}
