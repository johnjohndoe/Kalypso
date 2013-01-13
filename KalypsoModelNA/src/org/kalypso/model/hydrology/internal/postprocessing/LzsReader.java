/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.hydrology.internal.postprocessing;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.LineNumberReader;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.hydrology.binding.initialValues.IniHyd;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.NaCatchmentData;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Reads a lzs file.
 * 
 * @author Gernot Belger
 */
class LzsReader
{
  private static final Pattern PATTERN_HEADER_BODF = Pattern.compile( "([0-9]{8} [0-9]{2}) h ([0-9]+?) bodf" ); //$NON-NLS-1$

  private static enum CatchmentStatus
  {
    SEARCH_HEADER,
    READ_SNOW,
    READ_GWSP,
    READ_BODF,
  }

  private static final class State
  {
    private final int m_maxHydros;

    private final int m_counterHydros;

    private final CatchmentStatus m_status;

    public State( )
    {
      this( 0, CatchmentStatus.SEARCH_HEADER );
    }

    public State( final int maxHydros, final CatchmentStatus status )
    {
      this( maxHydros, 0, status );
    }

    private State( final int maxHydros, final int counterHydros, final CatchmentStatus status )
    {
      m_maxHydros = maxHydros;
      m_counterHydros = counterHydros;
      m_status = status;
    }

    public State increaseCounter( )
    {
      final int newCounter = m_counterHydros + 1;
      if( newCounter >= m_maxHydros )
        return new State();

      return new State( m_maxHydros, m_counterHydros + 1, m_status );
    }

    public State changeStatus( final CatchmentStatus status )
    {
      return new State( m_maxHydros, m_counterHydros, status );
    }

    public CatchmentStatus getStatus( )
    {
      return m_status;
    }
  }

  private final DateFormat m_dateFormat;

  private final org.kalypso.model.hydrology.binding.initialValues.Catchment m_iniCatchment;

  private final Catchment m_catchment;

  private final Date m_initialDate;

  private final NaCatchmentData m_catchmentData;

  public LzsReader( final NaCatchmentData catchmentData, final DateFormat dateFormat, final Date initialDate, final org.kalypso.model.hydrology.binding.initialValues.Catchment iniCatchment, final Catchment catchment )
  {
    m_catchmentData = catchmentData;
    m_dateFormat = dateFormat;
    m_initialDate = initialDate;
    m_iniCatchment = iniCatchment;
    m_catchment = catchment;
  }

  public IStatus read( final File lzsFile )
  {
    try( FileReader fileReader = new FileReader( lzsFile ) )
    {
      return readLzsFile( fileReader );
    }
    catch( final FileNotFoundException e )
    {
      final String message = Messages.getString( "org.kalypso.convert.namodel.manager.LzsimManager.27", m_catchment.getName() ); //$NON-NLS-1$
      return new Status( IStatus.WARNING, ModelNA.PLUGIN_ID, message, e );
    }
    catch( final Exception e )
    {
      final String message = String.format( Messages.getString( "LzsToGml.0" ), lzsFile.getName() ); //$NON-NLS-1$
      return new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, message, e );
    }
  }

  private IStatus readLzsFile( final FileReader fileReader ) throws Exception
  {
    final LineNumberReader reader = new LineNumberReader( fileReader );

    final String iniDate = m_dateFormat.format( m_initialDate );
    // TODO: why compare the datum? It would be more robust to just read all data that is in the lsz file, and append
    // it to the features!

    State state = new State();

    while( reader.ready() )
    {
      final String line = reader.readLine();
      if( line == null )
        break;

      final String cleanLine = line.trim().replaceAll( "\\s+", " " ); //$NON-NLS-1$ //$NON-NLS-2$
      state = parseLine( cleanLine, state, iniDate );
    }

    // TODO: if we reach this line without any read data, something is wrong!

    return Status.OK_STATUS;
  }

  private State parseLine( final String line, final State state, final String iniDate )
  {
    switch( state.getStatus() )
    {
      case SEARCH_HEADER:
        final Matcher matcherBODF = PATTERN_HEADER_BODF.matcher( line );
        if( line.endsWith( "snow" ) && line.startsWith( iniDate ) ) //$NON-NLS-1$
          return state.changeStatus( CatchmentStatus.READ_SNOW );
        else if( line.endsWith( "gwsp" ) && line.startsWith( iniDate ) ) //$NON-NLS-1$
          return state.changeStatus( CatchmentStatus.READ_GWSP );
        else if( matcherBODF.matches() && line.startsWith( iniDate ) )
        {
          final int maxHydros = Integer.parseInt( matcherBODF.group( 2 ) );
          return new State( maxHydros, CatchmentStatus.READ_BODF );
        }
        else
          return state.changeStatus( CatchmentStatus.SEARCH_HEADER );

      case READ_BODF:
      {
        final IFeatureBindingCollection<IniHyd> iniHyds = m_iniCatchment.getIniHyds();
        final IniHyd iniHyd = iniHyds.addNew( 0, IniHyd.FEATURE_INI_HYD, IniHyd.class );

        final String[] strings = line.split( " " ); //$NON-NLS-1$
        final int pos = Integer.parseInt( strings[0] ) - 1;

        final String hydroID = m_catchmentData.getHydroFeatureId( m_catchment, pos );
        iniHyd.setNaHydrotopID( hydroID );

        final Double interception = Double.valueOf( strings[1] );
        final List<Double> bofs = new ArrayList<>();
        for( int i = 2; i < strings.length; i++ )
        {
          final Double bf = Double.valueOf( strings[i] );
          bofs.add( bf );
        }

        iniHyd.setBi( interception );
        iniHyd.setBofs( bofs );

        return state.increaseCounter();
      }

      case READ_GWSP:
      {
        final String[] strings = line.split( " " ); //$NON-NLS-1$
        final Double hwgs = Double.valueOf( strings[1] );// hoehe gw
        final Double qb = Double.valueOf( strings[2] );// basisabfluss

        m_iniCatchment.setHwgs( hwgs );
        m_iniCatchment.setQb( qb );

        return state.changeStatus( CatchmentStatus.SEARCH_HEADER );
      }

      case READ_SNOW:
      {
        final String[] strings = line.split( " " ); //$NON-NLS-1$

        final Double h = Double.valueOf( strings[1] );// hoehe schnee
        final Double ws = Double.valueOf( strings[2] );// wassergehalt

        m_iniCatchment.setH( h );
        m_iniCatchment.setWS( ws );

        return state.changeStatus( CatchmentStatus.SEARCH_HEADER );
      }
    }

    throw new IllegalStateException();
  }
}