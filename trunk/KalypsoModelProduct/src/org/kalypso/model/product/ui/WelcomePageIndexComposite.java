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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.contribs.eclipse.swt.canvas.DefaultContentArea;
import org.kalypso.contribs.eclipse.swt.canvas.ImageCanvas2;
import org.kalypso.model.product.utils.MyColors;
import org.kalypso.model.product.utils.MyFonts;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.extension.IKalypsoModule;
import org.kalypso.project.database.client.extension.pages.module.IKalypsoModulePage;
import org.kalypso.project.database.client.extension.pages.welcome.IKalypsoModuleWelcomePageFrame;
import org.kalypso.project.database.client.extension.pages.welcome.IKalypsoWelcomePage;

/**
 * @author Dirk Kuch
 */
public class WelcomePageIndexComposite extends Composite
{
  protected int IMG_OFFSET = 0;

  protected final IKalypsoWelcomePage m_pageHandler;

  public WelcomePageIndexComposite( final Composite parent, final int style, final IKalypsoWelcomePage pageHandler )
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
    final ImageCanvas2 headerCanvas = new ImageCanvas2( this, SWT.NO_REDRAW_RESIZE );
    final GridData headerIconData = new GridData( GridData.FILL, GridData.FILL, true, false, 2, 0 );
    headerIconData.heightHint = headerIconData.minimumHeight = 110;
    headerCanvas.setLayoutData( headerIconData );

    /* main canvas */
    final ImageCanvas2 mainCanvas = new ImageCanvas2( this, SWT.NO_REDRAW_RESIZE );
    mainCanvas.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    final IKalypsoModule[] extensions = KalypsoProjectDatabaseClient.getKalypsoModules();
    for( final IKalypsoModule module : extensions )
    {
      final IKalypsoModuleWelcomePageFrame handler = module.getWelcomePageFrame();

      final DefaultContentArea content = new DefaultContentArea()
      {
        public int y_offset = (IMG_OFFSET += 80) - 80;

        @Override
        public java.awt.Point getContentAreaAnchorPoint( )
        {
          final Point canvasSize = mainCanvas.getSize();
          return new java.awt.Point( canvasSize.x / 2 - canvasSize.x / 8, y_offset );
        }
      };

      content.setImage( handler.getIcon() );
      content.setHoverImage( handler.getHoverIcon() );

      content.setText( handler.getLabel(), MyFonts.WELCOME_PAGE_MODULE, MyColors.COLOR_WELCOME_PAGE_HEADING, SWT.RIGHT );
      content.setMouseListener( new MouseAdapter()
      {
        /**
         * @see org.eclipse.swt.events.MouseAdapter#mouseUp(org.eclipse.swt.events.MouseEvent)
         */
        @Override
        public void mouseUp( final MouseEvent e )
        {
          final IKalypsoModulePage page = module.getModulePage();
          m_pageHandler.setPage( page );

          m_pageHandler.update();
        }
      }, handler.getTooltip() );

      mainCanvas.addContentArea( content );
    }

    /* reset IMG_OFFSET */
    IMG_OFFSET = 0;

    this.layout();
  }
}
