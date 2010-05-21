/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.risk.model.simulation;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import org.deegree.crs.transformations.CRSTransformation;
import org.deegree.model.spatialschema.ByteUtils;
import org.kalypso.grid.BinaryGeoGrid;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.SequentialBinaryGeoGridReader;
import org.kalypso.grid.SequentialBinaryGeoGridWriter;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.transformation.CachedTransformationFactory;
import org.kalypso.transformation.TransformUtilities;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.sort.SplitSort;
import org.kalypsodeegree_impl.model.sort.SplitSortSpatialIndex;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author barbarins
 * 
 */
public class ParallelDamageCalculationManager
{
  private final static int THREADS_AMOUNT = Runtime.getRuntime().availableProcessors();

  private Integer BLOCK_SIZE = 1024 * 1024 / 2;

  protected class CalculationBean
  {
    public byte[] m_blockData = new byte[BLOCK_SIZE];

    public boolean m_done = false;

    public int m_linePos = 0;

    public int m_bytesInBlock = 0;

    public BigDecimal m_max = BigDecimal.valueOf( -Double.MAX_VALUE );

    public BigDecimal m_min = BigDecimal.valueOf( Double.MAX_VALUE );
  }

  protected class CalculationWriterThread extends Thread
  {
    private ParallelDamageCalculationManager m_manager;

    private int m_nextBlockToBeWritten = 0;

    private int m_lastBlockToBeWritten = 0;

    // lets try without synchronizing ...
    public void incBlocksToProcessCount( )
    {
      m_lastBlockToBeWritten++;
    }

    public CalculationWriterThread( final ParallelDamageCalculationManager pManager )
    {
      m_manager = pManager;
    }

    @Override
    public void run( )
    {

      synchronized( this )
      {
        try
        {
          while( m_nextBlockToBeWritten < m_amountBlocks )
          {
            // System.out.println( "[Writer thread] sleeping... " );
            this.wait();
            // System.out.println( "[Writer thread] woke up! " );

            if( m_beans.isEmpty() == false )
            {
              for( ; m_nextBlockToBeWritten < m_lastBlockToBeWritten; m_nextBlockToBeWritten++ )
              {
                if( m_beans.get( m_nextBlockToBeWritten ).m_done == false )
                  break;

                // System.out.println( "[Writer thread] writing block " + m_nextBlockToBeWritten );
                CalculationBean lBean = m_beans.get( m_nextBlockToBeWritten );
                m_writer.write( lBean.m_blockData, lBean.m_bytesInBlock );
                
                m_writer.setMax( lBean.m_max );
                m_writer.setMin( lBean.m_min );

                m_beans.set( m_nextBlockToBeWritten, null );
              }
            }
            // System.out.println( "[Writer thread] Done writing for now ... " + m_nextBlockToBeWritten +
            // " total blocks:" + m_lastBlockToBeWritten );

          }
        }
        catch( IOException e )
        {
          e.printStackTrace();
        }
        catch( InterruptedException e )
        {
          e.printStackTrace();
        }

      }
    }
  }

  protected class CalculationJob extends Thread
  {
    private ParallelDamageCalculationManager m_manager;

    private CalculationBean m_bean;

    private Coordinate m_origin;

    private Coordinate m_offsetX;

    private Coordinate m_offsetY;

    private int m_inputScale;

    private int m_outputScale;
    
    public CalculationJob( final ParallelDamageCalculationManager pManager ) throws GeoGridException
    {
      m_manager = pManager;
      m_origin = m_reader.getDelegate().getOrigin();
      m_offsetX = m_reader.getDelegate().getOffsetX();
      m_offsetY = m_reader.getDelegate().getOffsetY();
      m_inputScale = m_manager.m_reader.getScale();
      m_outputScale = m_manager.m_writer.getScale();
    }

    @Override
    public void run( )
    {
      while( true )
      {
        m_bean = m_manager.getNextDataset( this );
        if( m_bean == null )
          break;
        operate();
      }

      // exit
    }

    private final GM_Position getPosition( final int x, final int y )
    {
      final double cx = m_origin.x + x * m_offsetX.x + y * m_offsetY.x;
      final double cy = m_origin.y + x * m_offsetX.y + y * m_offsetY.y;
      return GeometryFactory.createGM_Position( cx, cy );
    }

    private final double getValue( final int x, final int y, final int k )
    {
      // convert 4 bytes to integer
      final int z = (((m_bean.m_blockData[k - 3] & 0xff) << 24) | ((m_bean.m_blockData[k - 2] & 0xff) << 16) | ((m_bean.m_blockData[k - 1] & 0xff) << 8) | ((m_bean.m_blockData[k - 0] & 0xff)));

      if( z == Integer.MIN_VALUE /* NO_DATA */)
        return Double.NaN;

      final BigDecimal decimal = new BigDecimal( BigInteger.valueOf( z ), m_inputScale );
      final double value = decimal.doubleValue();

      // possible that waterdepth input grid contains water depth less than zero!
      if( value <= 0.0 )
        return Double.NaN;

      if( m_polygonCollection.size() == 0 )
        return Double.NaN;

      final GM_Position positionAt = getPosition( x, y );
      final GM_Position position = TransformUtilities.transform( positionAt, m_transformation );
      
      /* This list has some unknown cs. */

      final List<ILandusePolygon> list = m_polygonCollection.query( position );
      if( list == null || list.size() == 0 )
        return Double.NaN;
      else
      {
        for( final ILandusePolygon polygon : list )
        {
          if( polygon.contains( position ) )
          {
            final Integer landuseClassOrdinalNumber = polygon.getLanduseClassOrdinalNumber();
            final double damageValue = polygon.getDamageValue( value );

            if( Double.isNaN( damageValue ) )
              return Double.NaN;

            if( damageValue <= 0.0 )
              return Double.NaN;

            /* set statistic for landuse class */
            ILanduseClass landuseClass = m_landuseClasses.get( landuseClassOrdinalNumber );
            if( landuseClass == null )
              System.out.println( String.format( "Unknown landuse class: %s", landuseClassOrdinalNumber ) ); //$NON-NLS-1$
            else
              RiskModelHelper.fillStatistics( m_returnPeriod, landuseClass, damageValue, m_cellSize );
            return damageValue;
          }
        }
      }

      return Double.NaN;
    }

    private final void operate( )
    {
      int x = 0;
      int y = m_bean.m_linePos;

      final int maxX = (int) (m_lineLen / 4);
      
      // System.out.println( "Working on block: " + m_bean.m_linePos / m_linesInBlock);

      for( int k = 3; k <= m_bean.m_bytesInBlock; k = k + 4 )
      {
        // final double value = grid.getValueChecked( x, y );
        final double value = getValue( x, y, k );

        int intVal;
        if( Double.isNaN( value ) != true )
        {
          final BigDecimal scaled = BigDecimal.valueOf( value ).setScale( m_outputScale, BigDecimal.ROUND_HALF_UP );
          intVal = scaled.unscaledValue().intValue();

          final BigDecimal minmax = new BigDecimal( value ).setScale( 4, BigDecimal.ROUND_HALF_UP );
          
          m_bean.m_min = m_bean.m_min.min( minmax );
          m_bean.m_max = m_bean.m_max.max( minmax );
        } else {
          intVal = BinaryGeoGrid.NO_DATA;
        }


        // write the result back into the buffer
        ByteUtils.writeBEInt( m_bean.m_blockData, k - 3, intVal );

        x++;
        if( x == maxX )
        {
          x = 0;
          y++;
        }

      }
      // System.out.println( "Done with the block: " + m_bean.m_linePos / m_linesInBlock);
      m_bean.m_done = true;
    }
  }

  // Manager code below
  SequentialBinaryGeoGridReader m_reader;

  SequentialBinaryGeoGridWriter m_writer;

  IFeatureWrapperCollection<ILandusePolygon> m_polygonCollection;

  List<ILanduseClass> m_landuseClasses;

  double m_cellSize;

  int m_returnPeriod;

  protected static ArrayList<CalculationBean> m_beans;// = new

  private final static CalculationJob[] m_jobs = new CalculationJob[THREADS_AMOUNT];

  private final CalculationWriterThread m_writer_thread;

  static int m_amountBlocks;

  static int m_linesInBlock;

  private static int m_linesTotal;

  private static int m_linesRead = 0;

  static long m_lineLen = 0;

  CRSTransformation m_transformation;

  public ParallelDamageCalculationManager( SequentialBinaryGeoGridReader inputGridReader, SequentialBinaryGeoGridWriter outputGridWriter, IFeatureWrapperCollection<ILandusePolygon> polygonCollection, List<ILanduseClass> landuseClasses, int returnPeriod, double cellSize )
  {
    m_linesRead = 0;
    m_reader = inputGridReader;
    m_writer = outputGridWriter;
    m_polygonCollection = polygonCollection;
    m_landuseClasses = landuseClasses;
    m_cellSize = cellSize;
    m_returnPeriod = returnPeriod;
    m_writer_thread = new CalculationWriterThread( this );

    final ILandusePolygon landusePolygon = m_polygonCollection.get( 0 );
    final String coordinateSystem = landusePolygon.getGeometry().getCoordinateSystem();

    
    // calculate fitting block size
    try
    {
      // calculate the coordinate transformation
      m_transformation = CachedTransformationFactory.getInstance().createFromCoordinateSystems( m_reader.getDelegate().getSourceCRS(), coordinateSystem );

      m_linesTotal = inputGridReader.getSizeY();
      long linesPerThread = m_linesTotal / THREADS_AMOUNT;
      m_lineLen = inputGridReader.getSizeX() * 4;

      // block_size is set to "optimal" size of the buffer from start on
      m_linesInBlock = (int) (BLOCK_SIZE / m_lineLen);

      if( m_linesInBlock >= m_linesTotal )
        m_linesInBlock = (int) linesPerThread;

      if( m_linesInBlock == 0 )
        m_linesInBlock = 1;

      BLOCK_SIZE = m_linesInBlock * (int) m_lineLen * 4;

    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

  }

  public IGeoGrid getGrid( )
  {
    return m_reader.getDelegate();
  }

  public void calculate( ) throws IOException
  {
    m_amountBlocks = m_linesTotal / m_linesInBlock;
    if( m_linesTotal % m_linesInBlock != 0 )
      m_amountBlocks++;
    m_beans = new ArrayList<CalculationBean>( m_amountBlocks + 1 );

    try
    {
      for( int i = 0; i < (m_jobs.length); i++ )
      {
        m_jobs[i] = new CalculationJob( this );
        m_jobs[i].start();
      }

      m_writer_thread.start();

      for( int i = 0; i < (m_jobs.length); i++ )
      {
        m_jobs[i].join();
      }

      synchronized( m_writer_thread )
      {
        m_writer_thread.notify();
        m_writer_thread.join();
      }

      m_writer.close();
    }
    catch( InterruptedException e )
    {
      e.printStackTrace();
    }
    catch( GeoGridException e )
    {
      e.printStackTrace();
    }
  }

  public synchronized CalculationBean getNextDataset( final CalculationJob job )
  {
    // write down the results that might be ready for it
    try
    {

      if( m_linesRead >= m_linesTotal )
      {
        return null;
      }

      CalculationBean lBean = new CalculationBean();
      if( (m_linesRead + m_linesInBlock) <= m_linesTotal )
      {
        lBean.m_bytesInBlock = (int) (m_linesInBlock * m_lineLen);
      }
      else
      {
        lBean.m_bytesInBlock = (int) ((m_linesTotal - m_linesRead) * m_lineLen);
      }
      lBean.m_linePos = m_linesRead;
      m_reader.read( lBean.m_blockData, lBean.m_bytesInBlock );
      m_linesRead += m_linesInBlock;

      // write down ready results
      synchronized( m_writer_thread )
      {
        // System.out.println( "Notifying the writer!" );
        m_writer_thread.incBlocksToProcessCount();
        m_writer_thread.notify();
      }
      m_beans.add( lBean );
      return lBean;
    }
    catch( IOException e )
    {
      e.printStackTrace();
      return null;
    }
  }

}
