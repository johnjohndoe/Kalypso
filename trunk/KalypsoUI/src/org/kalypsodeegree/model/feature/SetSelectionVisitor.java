/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypsodeegree.model.feature;

import java.util.ArrayList;
import java.util.List;

import org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author doemming
 */
public class SetSelectionVisitor implements FeatureVisitor
{

  private final List m_listOfFeatureIds;

  private final IFeatureSelectionManager m_selectionManager;

  /**
   *  
   */
  public SetSelectionVisitor( List listOfFeatureIds, IFeatureSelectionManager selectionManager )
  {
    m_listOfFeatureIds = listOfFeatureIds==null ? new ArrayList(): listOfFeatureIds;
    m_selectionManager = selectionManager;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( Feature f )
  {
    if( m_listOfFeatureIds.contains( f.getId() ) )
      m_selectionManager.addToSelection( f );
    return true;
  }
}
