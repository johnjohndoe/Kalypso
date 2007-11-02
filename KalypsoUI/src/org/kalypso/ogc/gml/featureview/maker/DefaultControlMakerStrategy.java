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
package org.kalypso.ogc.gml.featureview.maker;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBElement;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.LayoutType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * This is one strategy to create a control. It uses several of the concrete makers in a defined order to create the
 * control.
 * 
 * @author Gernot Belger
 */
public class DefaultControlMakerStrategy implements IControlMaker
{
  private final List<IControlMaker> m_makers = new ArrayList<IControlMaker>();

  public DefaultControlMakerStrategy( final boolean addValidator, final boolean showTables, final boolean showButton )
  {
    // m_makers.add( new DefaultControlMaker( addValidator ) );
    m_makers.add( new DefaultFeatureControlMaker( addValidator ) );
    m_makers.add( new GuiHandlerControlMaker( addValidator ) );

    m_makers.add( new LinkedFeatureControlMaker( addValidator, showButton ) );
    m_makers.add( new LinkedListFeatureControlMaker( addValidator, showButton ) );
    m_makers.add( new TableControlMaker( showTables ) );
    m_makers.add( new InlineFeatureControlMaker() );

    m_makers.add( new ButtonControlMaker( addValidator ) );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.maker.IControlMaker#addControls(java.util.List,
   *      org.kalypso.template.featureview.LayoutType, org.kalypso.gmlschema.property.IPropertyType)
   */
  public boolean addControls( final List<JAXBElement< ? extends ControlType>> controlList, final LayoutType parentLayout, final IFeatureType ft, final IPropertyType ftp, final Feature feature ) throws AbortCreationException
  {
    for( final IControlMaker controlMaker : m_makers )
    {
      final boolean stop = controlMaker.addControls( controlList, parentLayout, ft, ftp, feature );
      if( stop )
        return true;
    }

    return false;
  }

}
