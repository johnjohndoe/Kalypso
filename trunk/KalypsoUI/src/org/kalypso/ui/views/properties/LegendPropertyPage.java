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
package org.kalypso.ui.views.properties;

import java.awt.Font;
import java.awt.Image;
import java.awt.image.BufferedImage;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.eclipse.ui.dialogs.PropertyPage;
import org.kalypso.contribs.eclipse.swt.awt.ImageConverter;
import org.kalypso.ogc.gml.IKalypsoTheme;

/**
 * This page will show a legend for a theme, if one is available.
 * 
 * @author Holger Albert
 */
public class LegendPropertyPage extends PropertyPage implements IWorkbenchPropertyPage
{
  /**
   * The constructor.
   */
  public LegendPropertyPage( )
  {
  }

  /**
   * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( Composite parent )
  {
    /* Get the theme. */
    IKalypsoTheme theme = getTheme();

    /* Create a composite. */
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 1, false ) );

    /* If there is no theme, no legend could be shown. */
    if( theme == null )
      return createError( composite, "Keine Legende verfügbar." );

    /* Get the legend graphic. */
    Image legendGraphic = null;

    try
    {
      /* Get the legend graphic. */
      legendGraphic = theme.getLegendGraphic( new Font( "Arial", Font.PLAIN, 10 ) );
    }
    catch( CoreException e )
    {
      return createError( composite, e.getLocalizedMessage() );
    }

    /* No legend available. */
    if( legendGraphic == null )
      return createError( composite, "Keine Legende verfügbar." );

    /* Convert to swt image date. */
    ImageData swtLegendData = ImageConverter.convertToSWT( (BufferedImage) legendGraphic );
    if( swtLegendData == null )
      return createError( composite, "Keine Legende verfügbar." );

    /* Create swt image. */
    org.eclipse.swt.graphics.Image swtLegend = new org.eclipse.swt.graphics.Image( getShell().getDisplay(), swtLegendData );

    /* Create a scrolled composite. */
    ScrolledComposite sComposite = new ScrolledComposite( composite, SWT.NONE );
    sComposite.setLayout( new FillLayout() );
    GridData sCompData = new GridData( SWT.FILL, SWT.FILL, true, true );
    sCompData.widthHint = 300;
    sComposite.setLayoutData( sCompData );

    /* And finally display it. TODO: Why are there no Scrollbars? */
    Canvas canvas = new Canvas( sComposite, SWT.NONE );
    canvas.setSize( swtLegend.getBounds().width, swtLegend.getBounds().height );
    canvas.setBackgroundImage( swtLegend );

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
  private Control createError( Composite parent, String message )
  {
    /* Create the label. */
    Label messageLabel = new Label( parent, SWT.NONE );
    GridData messageData = new GridData( SWT.FILL, SWT.NONE, true, false );
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
    IAdaptable element = getElement();
    IKalypsoTheme theme = (IKalypsoTheme) (element instanceof IKalypsoTheme ? element : element.getAdapter( IKalypsoTheme.class ));

    return theme;
  }
}