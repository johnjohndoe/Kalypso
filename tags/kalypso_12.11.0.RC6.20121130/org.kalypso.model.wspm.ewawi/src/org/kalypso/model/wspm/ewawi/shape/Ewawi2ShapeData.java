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
package org.kalypso.model.wspm.ewawi.shape;

import java.io.File;

/**
 * (Input-)Data for {@link Ewawi2ShapeOperation}.
 * 
 * @author Gernot Belger
 */
public class Ewawi2ShapeData
{
  private final File m_inputDir;

  private final File m_gewShape;

  private final File m_gewWidthShape;

  public Ewawi2ShapeData( final File inputDir, final File gewShape, final File gewWidthShape )
  {
    m_inputDir = inputDir;
    m_gewShape = gewShape;
    m_gewWidthShape = gewWidthShape;
  }

  public File getInputDir( )
  {
    return m_inputDir;
  }

  public File getGewShape( )
  {
    return m_gewShape;
  }

  public File getGewWidthShape( )
  {
    return m_gewWidthShape;
  }
}