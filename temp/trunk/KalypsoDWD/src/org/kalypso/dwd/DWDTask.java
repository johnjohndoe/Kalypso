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
package org.kalypso.dwd;

import java.io.File;
import java.net.URL;
import java.util.Date;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.kalypso.contribs.java.util.logging.ILogger;

/**
 * this task generates zml-files from
 * <li>lm-file (dwd-raster forecast), and</li>
 * <li>dwd2zmlconf.xml (mapping)</li>
 * <p>
 * TODO: add property for default filterstring <br>
 * TODO: add properties for timeintervalls (startsim,startforecast,stopsim) <br>
 * TODO: always update target, do not just overwrite
 * 
 * @author doemming
 */

public class DWDTask extends Task
{

  /** context for target hrefs */
  private File m_targetContext;

  /** mapping configuration */
  private URL m_dwd2zmlConfUrl;

  /** lm raster */
  private URL m_obsRasterURL;

  private long m_from;

  private long m_forecastFrom;

  private long m_to;

  private String m_filter;

  public final void setDwd2zmlConfUrl( final URL dwd2zmlConfUrl )
  {
    m_dwd2zmlConfUrl = dwd2zmlConfUrl;
  }

  public final void setObsRasterURL( final URL obsRasterURL )
  {
    m_obsRasterURL = obsRasterURL;
  }

  public final void setTargetContext( final File targetContext )
  {
    m_targetContext = targetContext;
  }

  /**
   * 
   * @param from
   *          beginning of measure periode
   */
  public final void setFrom( long from )
  {
    m_from = from;
  }

  /**
   * 
   * @param forecastFrom
   *          beginning of forecast periode (end of measure periode)
   *  
   */
  public final void setForecastFrom( long forecastFrom )
  {
    m_forecastFrom = forecastFrom;
  }

  /**
   * 
   * @param to
   *          end of forecast periode
   */
  public final void setTo( long to )
  {
    m_to = to;
  }

  public final void setZMLFilter( String filter )
  {
    m_filter = filter;
  }

  /**
   * 
   * @see org.apache.tools.ant.Task#execute()
   */
  public void execute() throws BuildException
  {
    try
    {
      final ILogger logger = new ILogger()
      {
        /**
         * @see org.kalypso.contribs.java.util.logging.ILogger#log(java.lang.String)
         */
        public void log( String message )
        {
          System.out.println( message );
        }
      };
      logger.log( "DWD-task: generates ZML files from DWD-forecast" );
      logger.log( "  inputRaster: " + m_obsRasterURL );
      logger.log( "  raster to zml mapping: " + m_dwd2zmlConfUrl );
      logger.log( "  context for targets (zml-files): " + m_targetContext );
      logger.log( "  unmarshall dwd2zml configuration ..." );

      final DWDTaskDelegate delegate = new DWDTaskDelegate();
      delegate.execute( logger, m_obsRasterURL, m_dwd2zmlConfUrl, m_targetContext, new Date( m_from ), new Date(
          m_forecastFrom ), new Date( m_to ), m_filter );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new BuildException( e.getLocalizedMessage(), e );
    }
  }
}
