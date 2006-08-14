/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.ogc.gml.util;

import java.util.Vector;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.restriction.EnumerationRestriction;
import org.kalypso.gmlschema.property.restriction.IRestriction;
import org.kalypso.gmlschema.property.restriction.MaxExclusiveRestriction;
import org.kalypso.gmlschema.property.restriction.MaxInclusiveRestriction;
import org.kalypso.gmlschema.property.restriction.MaxLengthRestriction;
import org.kalypso.gmlschema.property.restriction.MinExclusiveRestriction;
import org.kalypso.gmlschema.property.restriction.MinInclusiveRestriction;
import org.kalypso.gmlschema.property.restriction.MinLengthRestriction;
import org.kalypso.gmlschema.property.restriction.RegExpRestriction;
import org.kalypsodeegree.model.feature.Feature;

/**
 * This class serves as factory class. It shall provide created rules from a property. An other aspect is the check if
 * the content of the property is valid.
 * 
 * @author albert
 */
public abstract class RuleFactory
{
  private RuleFactory( )
  {
    super();
  }

  /**
   * This function creates a list of rules for a property, if it have any restrictions.
   * 
   * @param feature -
   *          The GML that represents the feature.
   * @param ftp
   *          The GML schema that represents a feature property.
   */
  public static IRule[] getRules( final Feature feature, final IPropertyType ftp )
  {
    Vector<IRule> rules = new Vector<IRule>();

    if( ftp instanceof IValuePropertyType )
    {
      final IValuePropertyType vpt = (IValuePropertyType) ftp;

      final IRestriction[] restrictions = vpt.getRestriction();

      for( int i = 0; i < restrictions.length; i++ )
      {
        IRestriction restriction = restrictions[i];

        /* What restrictions do I have found here? */

        /* MaxExclusiveRestriction. */
        if( restriction instanceof MaxExclusiveRestriction )
        {
          /* Add the Rule. */
          MaxExclusiveRestriction maxexclusiverestriction = (MaxExclusiveRestriction) restriction;
          rules.add( new MaxExclusiveRule( maxexclusiverestriction.getMaxExclusive() ) );
        }

        /* MinExclusvieRestriction. */
        if( restriction instanceof MinExclusiveRestriction )
        {
          /* Add the Rule. */
          MinExclusiveRestriction minexclusiverestriction = (MinExclusiveRestriction) restriction;
          rules.add( new MinExclusiveRule( minexclusiverestriction.getMinExclusive() ) );
        }

        /* MaxInclusiveRestriction. */
        if( restriction instanceof MaxInclusiveRestriction )
        {
          /* Add the Rule. */
          MaxInclusiveRestriction maxinclusiverestriction = (MaxInclusiveRestriction) restriction;
          rules.add( new MaxInclusiveRule( maxinclusiverestriction.getMaxInclusive() ) );
        }

        /* MinInclusvieRestriction. */
        if( restriction instanceof MinInclusiveRestriction )
        {
          /* Add the Rule. */
          MinInclusiveRestriction mininclusiverestriction = (MinInclusiveRestriction) restriction;
          rules.add( new MinInclusiveRule( mininclusiverestriction.getMinInclusive() ) );
        }

        /* RegExpRestriction. */
        if( restriction instanceof RegExpRestriction )
        {
          /* Add the Rule. */
          RegExpRestriction regexprestriction = (RegExpRestriction) restriction;
          rules.add( new RegExpRule( regexprestriction.getPatterns() ) );
        }

        /* MaxLengthRestriction. */
        if( restriction instanceof MaxLengthRestriction )
        {
          /* Add the Rule. */
          MaxLengthRestriction maxlengthrestriction = (MaxLengthRestriction) restriction;
          rules.add( new MaxLengthRule( maxlengthrestriction.getMaxLength() ) );
        }

        /* MinLengthRestriction. */
        if( restriction instanceof MinLengthRestriction )
        {
          /* Add the Rule. */
          MinLengthRestriction minlengthrestriction = (MinLengthRestriction) restriction;
          rules.add( new MinLengthRule( minlengthrestriction.getMinLength() ) );
        }

        /* EnumerationRestriction. */
        if( restriction instanceof EnumerationRestriction )
        {
          /* Add the Rule. */
          EnumerationRestriction enumerationrestriction = (EnumerationRestriction) restriction;
          rules.add( new EnumerationRule( enumerationrestriction.getEnumeration(), enumerationrestriction.getLabels() ) );
        }

        /* TODO: Add new rules here. */
      }
    }

    /* IsNillableRule. */
    rules.add( new IsNillableRule( ftp.isNillable(), ftp.getMinOccurs() ) );

    return rules.toArray( new IRule[] {} );
  }
}
