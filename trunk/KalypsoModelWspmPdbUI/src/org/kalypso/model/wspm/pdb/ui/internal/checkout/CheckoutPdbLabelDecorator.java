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
package org.kalypso.model.wspm.pdb.ui.internal.checkout;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.BaseLabelProvider;
import org.eclipse.jface.viewers.DecorationOverlayIcon;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILabelDecorator;
import org.eclipse.swt.graphics.Image;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages.IMAGE;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class CheckoutPdbLabelDecorator extends BaseLabelProvider implements ILabelDecorator
{
  private Set<Object> m_allPdbElementsWithWspm;

  private final Map<String, Image> m_images = new HashMap<>();

  @Override
  public void dispose( )
  {
    for( final Image image : m_images.values() )
      image.dispose();

    super.dispose();
  }

  @Override
  public Image decorateImage( final Image image, final Object element )
  {
    if( m_allPdbElementsWithWspm.contains( element ) )
      return getDecoratedImage( image, element );

    return image;
  }

  @Override
  public String decorateText( final String text, final Object element )
  {
    if( m_allPdbElementsWithWspm.contains( element ) )
      return String.format( Messages.getString( "CheckoutPdbLabelDecorator.0" ), text ); //$NON-NLS-1$

    return text;
  }

  private Image getDecoratedImage( final Image image, final Object element )
  {
    final String key = element.getClass().getName();
    if( !m_images.containsKey( key ) )
      m_images.put( key, createDecoratedImage( image ) );

    return m_images.get( key );
  }

  private Image createDecoratedImage( final Image image )
  {
    final ImageDescriptor warningImage = WspmPdbUiImages.getImageDescriptor( IMAGE.WARNING_DECORATION );

    final DecorationOverlayIcon decorated = new DecorationOverlayIcon( image, warningImage, IDecoration.BOTTOM_RIGHT );
    return decorated.createImage();
  }

  public void setElements( final Set<Object> allPdbElementsWithWspm )
  {
    m_allPdbElementsWithWspm = allPdbElementsWithWspm;
  }
}
