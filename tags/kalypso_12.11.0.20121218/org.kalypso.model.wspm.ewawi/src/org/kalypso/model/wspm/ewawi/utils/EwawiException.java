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
package org.kalypso.model.wspm.ewawi.utils;

/**
 * Exception that indicates an error with EWAWI+ data.
 * 
 * @author Holger Albert
 */
public class EwawiException extends Exception
{
  /**
   * The constructor.
   */
  public EwawiException( )
  {
  }

  /**
   * The constructor.
   * 
   * @param message
   *          The detail message. The detail message is saved for later retrieval by the getMessage() method.
   */
  public EwawiException( final String message )
  {
    super( message );
  }

  /**
   * The constructor.
   * 
   * @param cause
   *          The cause (which is saved for later retrieval by the getCause() method). (A null value is permitted, and
   *          indicates that the cause is nonexistent or unknown.)
   */
  public EwawiException( final Throwable cause )
  {
    super( cause );
  }

  /**
   * The constructor.
   * 
   * @param message
   *          The detail message. The detail message is saved for later retrieval by the getMessage() method.
   * @param cause
   *          The cause (which is saved for later retrieval by the getCause() method). (A null value is permitted, and
   *          indicates that the cause is nonexistent or unknown.)
   */
  public EwawiException( final String message, final Throwable cause )
  {
    super( message, cause );
  }
}