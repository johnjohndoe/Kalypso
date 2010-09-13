/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.core.profile.sinuositaet;

import org.kalypso.model.wspm.core.profil.IProfileObject;

/**
 * @author Dirk Kuch
 */
public interface ISinuositaetProfileObject extends IProfileObject
{
  public static final String ID = "urn:ogc:gml:dict:kalypso:model:wspm:tuhh:core:sinuositaetTypes#SINUSITAET"; //$NON-NLS-1$

  public static final String PROPERTY_KENNUNG = "urn:ogc:gml:dict:kalypso:model:wspm:tuhh:core:sinuositaetComponents#KENNUNG"; //$NON-NLS-1$

  public static final String PROPERTY_SN = "urn:ogc:gml:dict:kalypso:model:wspm:tuhh:core:sinuositaetComponents#SN"; //$NON-NLS-1$

  public static final String PROPERTY_GERINNE_ART = "urn:ogc:gml:dict:kalypso:model:wspm:tuhh:core:sinuositaetComponents#GERINNE_ART"; //$NON-NLS-1$

  public static final String PROPERTY_LF = "urn:ogc:gml:dict:kalypso:model:wspm:tuhh:core:sinuositaetComponents#LF"; //$NON-NLS-1$

  SINUOSITAET_KENNUNG getKennung( );

  Double getSinuositaet( );

  SINUOSITAET_GERINNE_ART getGerinneArt( );

  Double getLf( );

}
