/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.services.calculation.service.impl;

import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.service.CalcJobServerBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypso.simulation.core.simspec.DataType;
import org.kalypso.simulation.core.simspec.Modelspec;

/**
 * @author belger
 */
public class ModelspecData
{
  private final Modelspec m_modelspec;

  private final Map m_inputHash;

  private final Map m_outputHash;

  public ModelspecData( final URL modelspecUrl, final Unmarshaller unmarshaller ) throws CalcJobServiceException
  {
    try
    {
      m_modelspec = (Modelspec) unmarshaller.unmarshal( modelspecUrl );

      m_inputHash = createHash( m_modelspec.getInput() );
      m_outputHash = createHash( m_modelspec.getOutput() );
    }
    catch( final JAXBException e )
    {
      throw new CalcJobServiceException( "Modelspezifikation konnte nicht geladen werden.", e );
    }
  }

  private Map createHash( final List<DataType> list )
  {
    final HashMap<String, DataType> map = new HashMap<String, DataType>( list.size() );
    for( final DataType data : list )
      map.put( data.getId(), data );

    return map;
  }

  /**
   * Prüft, ob für alle benötigten ID eine eingabe da ist.
   * 
   * @throws CalcJobServiceException
   */
  public void checkInput( final ICalcDataProvider data ) throws CalcJobServiceException
  {
    for( final Iterator iIt = m_inputHash.values().iterator(); iIt.hasNext(); )
    {
      final DataType input = (DataType) iIt.next();
      final String id = input.getId();
      final String description = input.getDescription();
      if( !input.isOptional() && !data.hasID( id ) )
        throw new CalcJobServiceException( "Keine Eingangsdaten für ID " + id + " (" + description + ") vorhanden.", null );
    }
  }

  public CalcJobServerBean[] getInput( )
  {
    return toServerBeans( m_inputHash.values() );
  }

  public CalcJobServerBean[] getOutput( )
  {
    return toServerBeans( m_outputHash.values() );
  }

  private CalcJobServerBean[] toServerBeans( final Collection values )
  {
    final CalcJobServerBean[] beans = new CalcJobServerBean[values.size()];
    int count = 0;
    for( final Iterator iter = values.iterator(); iter.hasNext(); )
    {
      final DataType data = (DataType) iter.next();
      beans[count++] = new CalcJobServerBean( data.getId(), data.getDescription() );
    }

    return beans;
  }

  public boolean hasOutput( final String id )
  {
    return m_outputHash.containsKey( id );
  }
}