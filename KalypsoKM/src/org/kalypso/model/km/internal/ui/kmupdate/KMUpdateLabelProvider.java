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
package org.kalypso.model.km.internal.ui.kmupdate;

import org.eclipse.jface.viewers.LabelProvider;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author doemming
 */
public class KMUpdateLabelProvider extends LabelProvider
{
  /**
   * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
   */
  @Override
  public String getText( final Object element )
  {
    if( !(element instanceof KMChannel) )
      return element.toString();

    final KMChannel channel = (KMChannel) element;
    final String label = FeatureHelper.getAnnotationValue( channel, IAnnotation.ANNO_LABEL );

    return String.format( "%s (ID=#%s)", label, channel.getId() ); //$NON-NLS-1$
  }
}
