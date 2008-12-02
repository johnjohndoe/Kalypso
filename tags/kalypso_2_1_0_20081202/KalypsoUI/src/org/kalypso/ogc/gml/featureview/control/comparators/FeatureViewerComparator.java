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
package org.kalypso.ogc.gml.featureview.control.comparators;

import java.util.HashMap;

import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.kalypso.ui.editor.actions.FeatureComparator;
import org.kalypsodeegree.model.feature.Feature;

/**
 * This ViewerComparator has a init()-function, which will allow the client to send any parameters (out of a .gft, for
 * example) to the comparator, which he can use to compare the elements. He can also compare objects and not only
 * strings.
 * 
 * @author Holger Albert
 */
public class FeatureViewerComparator extends ViewerComparator implements IViewerComparator
{
  /**
   * Stores the parameter, if any are given.
   */
  private HashMap<String, String> m_params;

  /**
   * This comparator will be used to compare the elements, instead of the one of the parent.
   */
  private FeatureComparator m_comparator;

  /**
   * The constructor.
   */
  public FeatureViewerComparator( )
  {
    m_comparator = null;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.comparators.IViewerComparator#init(org.kalypsodeegree.model.feature.Feature,
   *      java.util.HashMap)
   */
  public void init( Feature parent, HashMap<String, String> params )
  {
    /* Store the parameter. */
    m_params = params;

    /* Read the parameter. */
    String namespaceURI = m_params.get( "namespaceURI" ); //$NON-NLS-1$
    String localPart = m_params.get( "localPart" ); //$NON-NLS-1$

    /* If both required parameter are given, create the comparator. */
    if( namespaceURI != null && localPart != null )
      m_comparator = new FeatureComparator( parent, new QName( namespaceURI, localPart ) );
  }

  /**
   * @see org.eclipse.jface.viewers.ViewerComparator#compare(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  @Override
  public int compare( Viewer viewer, Object e1, Object e2 )
  {
    if( m_comparator == null )
      return super.compare( viewer, e1, e2 );

    return m_comparator.compare( e1, e2 );
  }
}