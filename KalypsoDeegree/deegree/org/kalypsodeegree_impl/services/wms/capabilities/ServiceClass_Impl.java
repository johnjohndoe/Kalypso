/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.services.wms.capabilities;

import java.net.URL;

import org.deegree.services.wms.capabilities.ServiceClass;

/**
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-03-01
 */
public class ServiceClass_Impl implements ServiceClass
{
  private String className = null;

  private URL configurationFile = null;

  /**
   * Creates a new ServiceClass_Impl object.
   * 
   * @param className
   * @param configurationFile
   */
  ServiceClass_Impl( String className, URL configurationFile )
  {
    setClassName( className );
    setConfigurationFile( configurationFile );
  }

  /**
   * returns the name of the class that acts as service for a layer
   */
  public String getClassName()
  {
    return className;
  }

  /**
   * sets the name of the class that acts as filter service for a layer
   */
  public void setClassName( String className )
  {
    this.className = className;
  }

  /**
   * returns the name of the configuration file for the service class. the
   * definition of a configuration file is optional
   */
  public URL getConfigurationFile()
  {
    return configurationFile;
  }

  /**
   * sets the name of the configuration file for the service class. the
   * definition of a configuration file is optional
   */
  public void setConfigurationFile( URL configurationFile )
  {
    this.configurationFile = configurationFile;
  }
}