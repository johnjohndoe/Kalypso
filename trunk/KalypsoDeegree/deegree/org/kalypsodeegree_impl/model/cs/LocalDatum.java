/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 It has been implemented within SEAGIS - An OpenSource implementation of OpenGIS specification
 (C) 2001, Institut de Recherche pour le Développement (http://sourceforge.net/projects/seagis/)
 SEAGIS Contacts:  Surveillance de l'Environnement Assistée par Satellite
 Institut de Recherche pour le Développement / US-Espace
 mailto:seasnet@teledetection.fr


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
package org.deegree_impl.model.cs;

// OpenGIS dependencies
import java.util.Map;

import org.opengis.cs.CS_LocalDatum;

/**
 * Local datum. If two local datum objects have the same datum type and name,
 * then they can be considered equal. This means that coordinates can be
 * transformed between two different local coordinate systems, as long as they
 * are based on the same local datum.
 * 
 * @version 1.00
 * @author OpenGIS (www.opengis.org)
 * @author Martin Desruisseaux
 * 
 * @see org.opengis.cs.CS_LocalDatum
 */
public class LocalDatum extends Datum
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = 426762179497761085L;

  /**
   * Creates a local datum.
   * 
   * @param name
   *          Name to give new object.
   * @param type
   *          Type of local datum to create.
   *  
   */
  public LocalDatum( final String name, final DatumType.Local type )
  {
    super( name, type );
  }

  /**
   * Creates a local datum.
   * 
   * @param properties
   *          The set of properties (see {@link Info}).
   * @param type
   *          Type of local datum to create.
   */
  LocalDatum( final Map properties, final DatumType type )
  {
    super( properties, type );
  }

  /**
   * Gets the type of the datum as an enumerated code.
   * 
   * Note: return type will be changed to {@link DatumType.Local}when we will
   * be able to use generic types (with JDK 1.5).
   * 
   * @see org.opengis.cs.CS_LocalDatum#getDatumType()
   */
  public DatumType/* .Local */getDatumType()
  {
    return (DatumType.Local)super.getDatumType();
  }

  /**
   * Fill the part inside "[...]". Used for formatting Well Know Text (WKT).
   */
  String addString( final StringBuffer buffer )
  {
    super.addString( buffer );
    return "LOCAL_DATUM";
  }

  /**
   * Returns an OpenGIS interface for this datum. The returned object is
   * suitable for RMI use.
   * 
   * Note: The returned type is a generic {@link Object}in order to avoid too
   * early class loading of OpenGIS interface.
   */
  final Object toOpenGIS( final Object adapters )
  {
    return new Export( adapters );
  }

  /////////////////////////////////////////////////////////////////////////
  //////////////// ////////////////
  //////////////// OPENGIS ADAPTER ////////////////
  //////////////// ////////////////
  /////////////////////////////////////////////////////////////////////////

  /**
   * Wrap a {@link LocalDatum}object for use with OpenGIS. This class is
   * suitable for RMI use.
   * 
   * @version 1.0
   * @author Martin Desruisseaux
   */
  private final class Export extends Datum.Export implements CS_LocalDatum
  {
    /**
     * Construct a remote object.
     */
    protected Export( final Object adapters )
    {
      super( adapters );
    }
  }
}