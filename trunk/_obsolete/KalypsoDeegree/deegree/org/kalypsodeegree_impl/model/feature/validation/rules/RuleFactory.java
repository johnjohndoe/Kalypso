/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.model.feature.validation.rules;

import java.text.ParseException;
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
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;

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
   * @param ftp
   *            The GML schema that represents a feature property.
   */
  public static IRule[] getRules( final IPropertyType ftp )
  {
    final Vector<IRule> rules = new Vector<IRule>();

    if( ftp instanceof IValuePropertyType )
    {
      final IValuePropertyType vpt = (IValuePropertyType) ftp;

      final IRestriction[] restrictions = vpt.getRestriction();

      for( int i = 0; i < restrictions.length; i++ )
      {
        final IRestriction restriction = restrictions[i];

        /* What restrictions do I have found here? */

        /* MaxExclusiveRestriction. */
        if( restriction instanceof MaxExclusiveRestriction )
        {
          /* Add the Rule. */
          final MaxExclusiveRestriction maxexclusiverestriction = (MaxExclusiveRestriction) restriction;
          rules.add( new MaxExclusiveRule( maxexclusiverestriction.getMaxExclusive() ) );
        }

        /* MinExclusvieRestriction. */
        if( restriction instanceof MinExclusiveRestriction )
        {
          /* Add the Rule. */
          final MinExclusiveRestriction minexclusiverestriction = (MinExclusiveRestriction) restriction;
          rules.add( new MinExclusiveRule( minexclusiverestriction.getMinExclusive() ) );
        }

        /* MaxInclusiveRestriction. */
        if( restriction instanceof MaxInclusiveRestriction )
        {
          /* Add the Rule. */
          final MaxInclusiveRestriction maxinclusiverestriction = (MaxInclusiveRestriction) restriction;
          rules.add( new MaxInclusiveRule( maxinclusiverestriction.getMaxInclusive() ) );
        }

        /* MinInclusvieRestriction. */
        if( restriction instanceof MinInclusiveRestriction )
        {
          /* Add the Rule. */
          final MinInclusiveRestriction mininclusiverestriction = (MinInclusiveRestriction) restriction;
          rules.add( new MinInclusiveRule( mininclusiverestriction.getMinInclusive() ) );
        }

        /* RegExpRestriction. */
        if( restriction instanceof RegExpRestriction )
        {
          /* Add the Rule. */
          final RegExpRestriction regexprestriction = (RegExpRestriction) restriction;
          rules.add( new RegExpRule( regexprestriction.getPatterns() ) );
        }

        /* MaxLengthRestriction. */
        if( restriction instanceof MaxLengthRestriction )
        {
          /* Add the Rule. */
          final MaxLengthRestriction maxlengthrestriction = (MaxLengthRestriction) restriction;
          rules.add( new MaxLengthRule( maxlengthrestriction.getMaxLength() ) );
        }

        /* MinLengthRestriction. */
        if( restriction instanceof MinLengthRestriction )
        {
          /* Add the Rule. */
          final MinLengthRestriction minlengthrestriction = (MinLengthRestriction) restriction;
          rules.add( new MinLengthRule( minlengthrestriction.getMinLength() ) );
        }

        /* EnumerationRestriction. */
        if( restriction instanceof EnumerationRestriction )
        {
          /* Add the Rule. */
          final EnumerationRestriction enumerationrestriction = (EnumerationRestriction) restriction;
          final IMarshallingTypeHandler typeHandler = (IMarshallingTypeHandler) vpt.getTypeHandler();
          final Object[] enumValues = enumerationrestriction.getEnumeration();
          final Object[] enumValueObjects = new Object[enumValues.length];
          for( int j = 0; j < enumValues.length; j++ )
          {
            final String enumValue = enumValues[j].toString();
            try
            {
              enumValueObjects[j] = typeHandler.parseType( enumValue );
            }
            catch( final ParseException e )
            {
              // TODO Auto-generated catch block
              e.printStackTrace();
            }
          }

          rules.add( new EnumerationRule( enumValueObjects, enumerationrestriction.getLabels() ) );
        }

        /* TODO: Add new rules here. */
      }
    }

    /* IsNillableRule. */
    rules.add( new IsNillableRule( ftp.isNillable(), ftp.getMinOccurs() ) );

    return rules.toArray( new IRule[] {} );
  }
}
