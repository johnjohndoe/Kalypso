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
package org.kalypso.ui.catalogs;

import java.net.URL;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;

import org.kalypso.core.jaxb.TemplateUtilitites;
import org.kalypso.template.featureview.FeatureviewType;

/**
 * @author Gernot Belger
 */
public class FeatureTypeFeatureviewCatalog extends FeatureTypeCatalog
{
  private static final String BASETYPE = "featureview"; //$NON-NLS-1$

  @SuppressWarnings("unchecked") //$NON-NLS-1$
  public static FeatureviewType getFeatureview( final URL context, final QName qname ) throws JAXBException
  {
    final URL url = getURL( BASETYPE, context, qname );
    if( url == null )
      return null;

    final Unmarshaller unmarshaller = TemplateUtilitites.createFeatureviewUnmarshaller();
    
    final JAXBElement<FeatureviewType> element = (JAXBElement<FeatureviewType>) unmarshaller.unmarshal( url );
    return element.getValue();
  }
  

  
}
