/*******************************************************************************
 * CVRange
 *
 * @author ETj
 */

package org.deegree_impl.model.cv;

import org.deegree.services.RangeParamList;
import org.deegree_impl.services.RangeParam;

import java.util.ArrayList;
import org.deegree.model.coverage.Level;

/**
 * A CVRange declares a Level valid for a certain dimension range.
 * A CVRange may contain other range axis.
 *
 * @author ETj
 */
public abstract class CVRange
{
	private String 		_name ;
	private Level 		_level;
	private ArrayList 	_subranges;
		
//	public static CVange newInstance( String name, String value, Level level, ArrayList subranges )
//	{
//		return newInstanceAux(name, value, level, subranges);
//	}
//
//	protected abstract CVRange newInstanceAux( String name, String value, Level level, ArrayList subranges );
//	public    abstract String   getRangeName();
	
	protected CVRange( String name, String value, Level level, ArrayList subranges )
	{
		_name 		= name;
		_level 		= level;
		_subranges 	= subranges;
	
		setValue(value);
	}
	
	public String 		getName()		{ return _name;	}
	public Level 		getLevel() 		{ return _level; }
	public ArrayList 	getSubranges() 	{ return _subranges;}
	
	abstract protected void   setValue(String value);
	abstract Object getValue();

	public String toString()
	{
		return toString("   ").toString();
	}
	
	protected StringBuffer toString(String indent)
	{
		StringBuffer sb = new StringBuffer();
		sb.append(indent).append("Range[name=").append(_name).append(",");
		sb.append("level=").append(_level).append(",");
		sb.append("#sub=").append(_subranges.size()).append("] {");
		
		for(int i=0; i<_subranges.size(); i++)
		{
			sb.append(((CVRange)_subranges.get(i)).toString(indent+"   "));
		}
				
		sb.append(indent).append("}\n");
		
		return sb;
	}

	/**
	 * Check if a range parameter match this CVRange
	 *
	 * @param    param               a  RangeParam
	 *
	 * @return   true if the data requested by param is into this CVRange
	 *
	 */
	public abstract boolean match(RangeParam param);

	
	/**
	 * Replaces the known tokens with values instantiated in the proper RangeParam
	 *
	 * @param    rpl                 a  RangeParamList
	 * @param    stringWithTokens    a  String
	 *
	 * @return   a String with known tokens replaced
	 *
	 */
	public abstract String substToken(RangeParamList rpl, String stringWithTokens);
		
}

