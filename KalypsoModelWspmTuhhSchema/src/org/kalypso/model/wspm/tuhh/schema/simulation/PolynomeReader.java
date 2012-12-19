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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResult;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.simulation.core.util.LogHelper;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;

/**
 * @author Gernot Belger
 */
public class PolynomeReader
{
  private final LogHelper m_log;

  private final QIntervalIndex m_intervalIndex;

  public PolynomeReader( final QIntervalIndex intervalIndex, final LogHelper log )
  {
    m_intervalIndex = intervalIndex;
    m_log = log;
  }

  public void read( final File polyFile ) throws IOException
  {
    LineNumberReader reader = null;
    try
    {
      reader = new LineNumberReader( new FileReader( polyFile ) );

      while( reader.ready() )
      {
        final String line = reader.readLine();
        if( line == null )
          break;

        final String trimmedLine = line.trim().replaceAll( " \\(h\\)", "\\(h\\)" ); //$NON-NLS-1$ //$NON-NLS-2$
        final String[] tokens = trimmedLine.split( " +" ); //$NON-NLS-1$
        if( tokens.length < 8 )
          continue;

        /* Determine if this is a good line: good lines are lines whos first token is a number */
        final BigDecimal station;
        try
        {
          station = new BigDecimal( tokens[0] );
        }
        catch( final NumberFormatException nfe )
        {
          /* Just ignore this line */
          continue;
        }

        try
        {
          final String description = tokens[1];
          // final String whatIsN = tokens[2];
          final char type = tokens[3].charAt( 0 );

          final int order = Integer.parseInt( tokens[4] );
          final double rangeMin = Double.parseDouble( tokens[5].replace( 'D', 'E' ) );
          final double rangeMax = Double.parseDouble( tokens[6].replace( 'D', 'E' ) );

          if( tokens.length < 7 + order + 1 )
          {
            /* A good line but bad content. Give user a hint that something might be wrong. */
            m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.22" ), polyFile.getName(), reader.getLineNumber() ); //$NON-NLS-1$
            continue;
          }

          final List<Double> coefficients = new ArrayList<>( order );
          for( int i = 7; i < 7 + order + 1; i++ )
          {
            final double coeff = Double.parseDouble( tokens[i].replace( 'D', 'E' ) );
            coefficients.add( coeff );
          }

          final Double[] doubles = coefficients.toArray( new Double[coefficients.size()] );
          final double[] coeffDoubles = ArrayUtils.toPrimitive( doubles );

          final String domainId;
          final String rangeId = IWspmTuhhQIntervallConstants.DICT_PHENOMENON_WATERLEVEL;
          switch( type )
          {
            case 'Q':
              domainId = IWspmTuhhQIntervallConstants.DICT_PHENOMENON_RUNOFF;
              break;
            case 'A':
              domainId = IWspmTuhhQIntervallConstants.DICT_PHENOMENON_AREA;
              break;
            case 'a':
              domainId = IWspmTuhhQIntervallConstants.DICT_PHENOMENON_ALPHA;
              break;

            default:
              m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.23" ), station ); //$NON-NLS-1$
              continue;
          }

          /* find feature for station */
          final QIntervallResult qresult = m_intervalIndex.get( station );
          if( qresult == null )
            m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.24" ), station, line ); //$NON-NLS-1$
          else
          {
            /* create new polynome */
            final IPolynomial1D poly1d = qresult.createPolynomial();

            poly1d.setName( description );
            poly1d.setDescription( description );
            poly1d.setCoefficients( coeffDoubles );
            poly1d.setRange( rangeMin, rangeMax );

            poly1d.setDomainPhenomenon( domainId );
            poly1d.setRangePhenomenon( rangeId );
          }
        }
        catch( final NumberFormatException nfe )
        {
          /* A good line but bad content. Give user a hint that something might be wrong. */
          m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.25" ), polyFile.getName(), reader.getLineNumber(), nfe.getLocalizedMessage() ); //$NON-NLS-1$
        }
        catch( final Exception e )
        {
          // should never happen
          m_log.log( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.25" ), polyFile.getName(), reader.getLineNumber(), e.getLocalizedMessage() ); //$NON-NLS-1$
        }
      }
      reader.close();
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

}
