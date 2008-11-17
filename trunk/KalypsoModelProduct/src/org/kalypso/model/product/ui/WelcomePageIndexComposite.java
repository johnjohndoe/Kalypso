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
package org.kalypso.model.product.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.contribs.eclipse.swt.canvas.HyperCanvas;
import org.kalypso.contribs.eclipse.swt.canvas.IHyperCanvasSizeHandler;
import org.kalypso.kalypsosimulationmodel.KalypsoModelSimulationBase;
import org.kalypso.kalypsosimulationmodel.extension.IKalypsoModule;
import org.kalypso.kalypsosimulationmodel.extension.IKalypsoModuleEnteringPageHandler;
import org.kalypso.kalypsosimulationmodel.extension.IKalypsoModulePageHandler;
import org.kalypso.kalypsosimulationmodel.extension.IKalypsoModuleWelcomePageHandler;
import org.kalypso.model.product.utils.MyColors;
import org.kalypso.model.product.utils.MyFonts;

/**
 * @author Dirk Kuch
 */
public class WelcomePageIndexComposite extends Composite
{
  protected int IMG_OFFSET = 0;

  protected final IKalypsoModulePageHandler m_pageHandler;

  public WelcomePageIndexComposite( final Composite parent, final int style, final IKalypsoModulePageHandler pageHandler )
  {
    super( parent, style );
    m_pageHandler = pageHandler;

    final GridLayout layout = new GridLayout( 2, false );
    layout.horizontalSpacing = 100;
    layout.verticalSpacing = 25;
    layout.marginWidth = 75;

    this.setLayout( layout );

    update();
  }

  /**
   * @see org.eclipse.swt.widgets.Control#update()
   */
  @Override
  public void update( )
  {
    /* header */
    // icon / button
    final HyperCanvas headerIcon = new HyperCanvas( this, SWT.NO_REDRAW_RESIZE );
    final GridData headerIconData = new GridData( GridData.FILL, GridData.FILL, true, false, 2, 0 );
    headerIconData.heightHint = headerIconData.minimumHeight = 110;
    headerIcon.setLayoutData( headerIconData );

    /* main canvas */
    final HyperCanvas mainCanvas = new HyperCanvas( this, SWT.NO_REDRAW_RESIZE );
    mainCanvas.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    final IKalypsoModule[] extensions = KalypsoModelSimulationBase.getKalypsoModules();
    for( final IKalypsoModule module : extensions )
    {
      final IKalypsoModuleWelcomePageHandler handler = module.getWelcomePageHandler();

      final IHyperCanvasSizeHandler iconSizeHandler = new IHyperCanvasSizeHandler()
      {
        public int m_y0 = -1;

        @Override
        public int getX( )
        {
          final Point canvasSize = mainCanvas.getSize();

          // fixed start position
          final int x = canvasSize.x / 2 - canvasSize.x / 8;
          return x;
        }

        @Override
        public int getY( )
        {
          if( m_y0 == -1 )
          {
            final Rectangle imgBounds = handler.getIcon().getBounds();
            m_y0 = IMG_OFFSET;
            IMG_OFFSET += imgBounds.height + 15;

            return m_y0;
          }
          else
            return m_y0;
        }
      };

      mainCanvas.addImage( handler.getIcon(), iconSizeHandler );
      mainCanvas.addText( handler.getLabel(), MyFonts.WELCOME_PAGE_MODULE, MyColors.COLOR_WELCOME_PAGE_HEADING, new IHyperCanvasSizeHandler()
      {
        @Override
        public int getX( )
        {
          return iconSizeHandler.getX() + 120;
        }

        @Override
        public int getY( )
        {
          final Rectangle imgBounds = handler.getIcon().getBounds();

          return iconSizeHandler.getY() + imgBounds.height / 2 - 7;
        }
      } );

      mainCanvas.addMouseListener( new MouseAdapter()
      {
        /**
         * @see org.eclipse.swt.events.MouseAdapter#mouseUp(org.eclipse.swt.events.MouseEvent)
         */
        @Override
        public void mouseUp( final MouseEvent e )
        {
          final IKalypsoModuleEnteringPageHandler page = module.getModuleEnteringPage();
          m_pageHandler.setPage( page );

          m_pageHandler.update();
        }
      }, handler.getIcon(), handler.getTooltip() );

    }

    /* reset IMG_OFFSET */
    IMG_OFFSET = 0;

    this.layout();
  }
}
