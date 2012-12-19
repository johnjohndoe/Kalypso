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
package org.kalypso.model.wspm.pdb.db.utils;

import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;

import javax.persistence.Column;

import org.eclipse.core.databinding.beans.BeanProperties;
import org.eclipse.core.databinding.beans.IBeanValueProperty;

/**
 * General utilities for the pdb mapping classes.
 * 
 * @author Gernot Belger
 */
public final class PdbMappingUtils
{
  private PdbMappingUtils( )
  {
    throw new UnsupportedOperationException();
  }

  private static Method getReadMethod( final Class< ? > type, final String propertyName )
  {
    final IBeanValueProperty property = BeanProperties.value( type, propertyName );

    final PropertyDescriptor descriptor = property.getPropertyDescriptor();

    final Method getMethod = descriptor.getReadMethod();
    return getMethod;
  }

  private static Column getColumAnnotation( final Class< ? > type, final String propertyName )
  {
    final Method getMethod = getReadMethod( type, propertyName );

    final Column column = getMethod.getAnnotation( Column.class );
    return column;
  }

  /**
   * Find the 'precision' annotation value for a given property from our mapping classes.
   */
  public static int findScale( final Class< ? > type, final String propertyName )
  {
    final Column column = getColumAnnotation( type, propertyName );
    return column.scale();
  }

  public static int findLength( final Class< ? > type, final String propertyName )
  {
    final Column column = getColumAnnotation( type, propertyName );
    return column.length();
  }
}