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
package org.kalypso.model.km.internal.binding;

import java.io.File;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.model.hydrology.binding.model.NaModell;

import com.sun.xml.bind.marshaller.NamespacePrefixMapper;

import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovGroupType;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.ObjectFactory;

/**
 * @author Gernot Belger
 */
public final class KMBindingUtils
{
  public final static String KM_NAMESPACE = "http://www.kalypso.wb.tu-harburg.de/rrm/kalininmiljukov"; //$NON-NLS-1$

  public final static ObjectFactory OF = new ObjectFactory();

  public final static JAXBContext JC = JaxbUtilities.createQuiet( ObjectFactory.class );

  private KMBindingUtils( )
  {
    throw new UnsupportedOperationException( "Helper class, do not instantiate" ); //$NON-NLS-1$
  }

  public static KalininMiljukovGroupType toKMConfiguration( final NaModell naModel )
  {
    final KMChannel[] kmChannels = naModel.getKMChannels();

    final KalininMiljukovGroupType kmGroup = OF.createKalininMiljukovGroupType();
    final List<KalininMiljukovType> kalininMiljukovList = kmGroup.getKalininMiljukov();

    for( final KMChannel channel : kmChannels )
    {
      final KalininMiljukovType km = createKMForFeature( channel );
      kalininMiljukovList.add( km );
    }

    return kmGroup;
  }

  private static KalininMiljukovType createKMForFeature( final KMChannel feature )
  {
    final KalininMiljukovType km = OF.createKalininMiljukovType();
    km.setId( feature.getId() );
    km.setFilePattern( "*km" ); //$NON-NLS-1$
    km.setPath( "" ); //$NON-NLS-1$

    final Double kmStart = feature.getKMStart();
    km.setKmStart( kmStart );

    final Double propEnd = feature.getKMEnd();
    km.setKmEnd( propEnd );

    return km;
  }

  public static KalininMiljukovGroupType load( final File file ) throws JAXBException
  {
    final Unmarshaller unmarshaller = JC.createUnmarshaller();
    final Object object = unmarshaller.unmarshal( file );
    if( object instanceof JAXBElement )
    {
      final JAXBElement< ? > object2 = (JAXBElement< ? >) object;
      return (KalininMiljukovGroupType) object2.getValue();
    }

    throw new JAXBException( "Unexpected root element: " + object ); //$NON-NLS-1$
  }

  public static void save( final KalininMiljukovGroupType kmGroup, final File file ) throws JAXBException
  {
    final Marshaller marshaller = JaxbUtilities.createMarshaller( JC );
    marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    marshaller.setProperty( Marshaller.JAXB_ENCODING, "UTF-8" ); //$NON-NLS-1$

    final NamespacePrefixMapper prefixMapper = new NamespacePrefixMapper()
    {
      @Override
      public String getPreferredPrefix( final String namespaceUri, final String suggestion, final boolean requirePrefix )
      {
        if( KM_NAMESPACE.equals( namespaceUri ) )
          return ""; //$NON-NLS-1$

        return null;
      }
    };


    marshaller.setProperty( "com.sun.xml.bind.namespacePrefixMapper", prefixMapper ); //$NON-NLS-1$

    final JAXBElement<KalininMiljukovGroupType> element = OF.createKalininMiljukovGroup( kmGroup );
    marshaller.marshal( element, file );
  }

}
