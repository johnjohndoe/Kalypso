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
package org.kalypso.ui.views.properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.eclipse.ui.dialogs.PropertyPage;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoTheme;

/**
 * This page will show a legend for a theme, if one is available.
 * 
 * @author Holger Albert
 */
public class LegendPropertyPage extends PropertyPage implements IWorkbenchPropertyPage
{
  /**
   * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parent )
  {
    /* Get the theme. */
    final IKalypsoTheme theme = getTheme();

    /* Create a composite. */
    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 1, false ) );

    /* If there is no theme, no legend could be shown. */
    if( theme == null )
      return createError( composite, Messages.getString("org.kalypso.ui.views.properties.LegendPropertyPage.0") ); //$NON-NLS-1$

    /* Get the legend graphic. */

    try
    {
      /* Get the legend graphic. */
      final Font font = new Font( composite.getDisplay(), "Arial", 10, SWT.NORMAL );
      final Image legendGraphic = theme.getLegendGraphic( font );
      /* No legend available. */
      if( legendGraphic == null )
        return createError( composite, Messages.getString("org.kalypso.ui.views.properties.LegendPropertyPage.2") ); //$NON-NLS-1$

      /* Create a group. */
      final Composite group = new Composite( composite, SWT.BORDER );
      group.setLayout( new GridLayout( 1, false ) );
      group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
      group.setBackground( group.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );

      /* Create a scrolled composite. */
      final ScrolledComposite sComposite = new ScrolledComposite( group, SWT.H_SCROLL | SWT.V_SCROLL );
      sComposite.setLayout( new GridLayout( 1, false ) );
      final GridData sCompData = new GridData( SWT.FILL, SWT.FILL, true, true );
      sComposite.setLayoutData( sCompData );
      sComposite.setBackground( sComposite.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );

      /* And finally display it. */
      final Canvas canvas = new Canvas( sComposite, SWT.NONE );
      final Rectangle legendBounds = legendGraphic.getBounds();
      canvas.setSize( legendBounds.width, legendBounds.height );
      canvas.setBackgroundImage( legendGraphic );

      sComposite.setContent( canvas );

      canvas.addDisposeListener( new DisposeListener()
      {
        @Override
        public void widgetDisposed( final DisposeEvent e )
        {
          legendGraphic.dispose();
          font.dispose();
        }
      } );

    }
    catch( final CoreException e )
    {
      return createError( composite, e.getLocalizedMessage() );
    }

    return composite;
  }

  /**
   * This function creates an error message in the given composite.
   * 
   * @param parent
   *            The parent composite.
   * @param message
   *            The error message.
   */
  private Control createError( final Composite parent, final String message )
  {
    /* Create the label. */
    final Label messageLabel = new Label( parent, SWT.NONE );
    final GridData messageData = new GridData( SWT.FILL, SWT.NONE, true, false );
    messageData.widthHint = 300;
    messageLabel.setLayoutData( messageData );

    /* Set the message. */
    messageLabel.setText( message );

    return parent;
  }

  /**
   * This function returns the theme.
   * 
   * @return The theme.
   */
  private IKalypsoTheme getTheme( )
  {
    final IAdaptable element = getElement();
    final IKalypsoTheme theme = (IKalypsoTheme) (element instanceof IKalypsoTheme ? element : element.getAdapter( IKalypsoTheme.class ));

    return theme;
  }
}