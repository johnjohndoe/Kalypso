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
/*
 * Created on 12.09.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.rulePattern;

import java.util.ArrayList;
import java.util.HashMap;

import org.deegree.graphics.sld.Rule;

/**
 * @author F.Lindemann
 *  
 */
/*
 * This class is fed with rules. It identifies whether it is a normal rule or
 * the rule belongs to a pattern. It collects the rules and returns the number
 * of rule items (-> number of tabitems to be displayed) as a list of Rule and
 * RuleCollection Objects.
 */

public class RuleFilterCollection
{
  private HashMap patterns = null;

  private ArrayList filteredRuleCollection = null;

  private RuleFilterCollection()
  {
    patterns = new HashMap();
    filteredRuleCollection = new ArrayList();
  }

  public static RuleFilterCollection getInstance()
  {
    return new RuleFilterCollection();
  }

  public void addRule( Rule rule )
  {
    // the name of a rule serves as key for the hashMap
    String key = rule.getName();
    // it it is a pattern, add to ruleCollection
    if( key != null && key.startsWith( "-name-" ) )
    {
      // 1. check whether there is already a rule collection with this rule
      if( patterns.containsKey( key ) )
      {
        // if yes - add rule to collection
        ( (RuleCollection)patterns.get( key ) ).addRule( rule );
      }
      else
      {
        RuleCollection ruleCollection = RuleCollection.getInstance( rule );
        patterns.put( key, ruleCollection );
        filteredRuleCollection.add( ruleCollection );
      }
    }
    else
    {
      filteredRuleCollection.add( rule );
    }
  }

  public void removeRule( Rule rule )
  {
    // the title of a rule serves as key for the hashMap
    String key = rule.getName();
    // it it is a pattern, add to ruleCollection
    if( key != null && key.startsWith( "-name-" ) )
    {
      // 1. check whether there is already a rule collection with this rule
      if( patterns.containsKey( key ) )
      {
        // if yes - add rule to collection
        ( (RuleCollection)patterns.get( key ) ).removeRule( rule );
      }
    }
    else
    {
      filteredRuleCollection.remove( rule );
    }
  }

  public void removeRuleCollection( RuleCollection coll )
  {
    filteredRuleCollection.remove( coll );
    patterns.remove( coll.getId() );
  }

  public ArrayList getFilteredRuleCollection()
  {
    return filteredRuleCollection;
  }

  public int size()
  {
    return filteredRuleCollection.size();
  }
}