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
package org.deegree_impl.model.ct;

/**
 * Common superclass for a number of transformation-related exceptions. It may
 * be thrown by {@link MathTransform}when a coordinate can't be transformed. It
 * may also be thrown when a coordinate transformation can't be created or
 * inverted.
 * 
 * @version 1.0
 * @author André Gosselin
 * @author Martin Desruisseaux
 */
public class TransformException extends Exception
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = -6311418979456076140L;

  /**
   * Constructs a new exception with no detail message.
   */
  public TransformException()
  {}

  /**
   * Constructs a new exception with the specified detail message.
   */
  public TransformException( final String message )
  {
    super( message );
  }

  /**
   * Constructs a new exception with the specified detail message and cause.
   */
  public TransformException( final String message, final Throwable cause )
  {
  /*
   * //----- BEGIN JDK 1.4 DEPENDENCIES ----- super(message, cause); /*----- END
   * OF JDK 1.4 DEPENDENCIES ---- super(message); ------- END OF JDK 1.3
   * FALLBACK --------
   */
  }
}