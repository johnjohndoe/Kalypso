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
package org.kalypso.model.wspm.ui.view.map;

import javax.xml.namespace.QName;

import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.ogc.gml.map.widgets.SelectionWidget;
import org.kalypso.ogc.gml.map.widgets.changers.SingleSelectionChanger;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.SelectFeaturesMapFunction;
import org.kalypso.ogc.gml.map.widgets.providers.QNameFeaturesProvider;

/**
 * @author Gernot Belger
 */
public class SelectionOneProfileWidget extends SelectionWidget
{
  public SelectionOneProfileWidget( )
  {
    super( Messages.SelectionOneProfileWidget_0, Messages.SelectionOneProfileWidget_1, new SelectFeaturesMapFunction( SelectFeaturesMapFunction.DEFAULT_RADIUS, new QNameFeaturesProvider( new QName( "org.kalypso.model.wspmprofile", "Profile" ) ), new SingleSelectionChanger( true ), KalypsoCorePlugin.getDefault().getSelectionManager() ), null ); //$NON-NLS-3$ //$NON-NLS-4$
  }
}
