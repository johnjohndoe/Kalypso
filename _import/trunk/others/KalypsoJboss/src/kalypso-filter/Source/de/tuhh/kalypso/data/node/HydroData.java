package de.tuhh.kalypso.data.node;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;

import de.tuhh.kalypso.util.Printf;

/** The class HydroData.java contains all hydrologically relevant data for a node.
 * There are six different Types of in- or outflowing water to or form a  node.
 * <UL>
 * <LI> Inflow to node
 * <LI> Outflow to a target node
 * <LI> Overflow from this node to a target node at a certain discharge.
 * <LI> Inflow to this node, not constant but descirbed by a function q(t).
 * <LI> Outflow from this node to a target node, not constant but descirbed by a function q(t).
 * <LI> Outflow from this node to a target node with a certain percentage.
 *</UL>
 * None, all or a combination of these types can be assigned to a node.
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version Strand.java,v 1.0 2002/07/01
 */



public class HydroData
{
	/** This field holds the data for a constant inflow to a node ( no source node ).
	 * @param inflowToNode*/
	public InToNode inflowToNode = null;
	/** This field holds the data for a constant outflow to a node ( traget node ).
	 * @param inflowToNode*/
	public OutToNode outflowToNode = null;
	/** This field holds the data for a constant outflow to a node ( traget node )
	 * once a certain discharge has been reached.
	 * @param inflowToNode*/
	public OverFlow overFlow = null;
	/** This field holds the data for a variing inflow to a node ( target node ).
	 * @param inflowToNode*/
	public InToNodeFunct inflowToNodeFunct = null;
	/** This field holds the data for a variing inflow to a node ( traget node ).
	 * @param inflowToNode*/
	public OutToNodeFunct outflowToNodeFunct = null;
	/** This field holds the data for a constant outflow to a node ( traget node )
	 * extracting a defined percentage of the flow.
	 * @param inflowToNode*/
	public PercentExtract precentExtract = null;
	
	public HydroData(){};
	public	class OverFlow
	{
		public double value = -1.0;
		public Node targetNode = null;
		
		public OverFlow(){};
		public OverFlow( double val, Node node )
		{
			this.value = val;
			this.targetNode = node;
		}
	}// inner class OverFlow
	/** This inner class holds the data for a constant inflow. Inflow in m³/s.*/
	public	class InToNode
	{
		/** Value holding the inflow into this node.
		 * @param value Default value: -1.0*/
		public double value = -1.0;
		
		public InToNode(){};
		public InToNode ( double value )
		{
			this.value = value;
		}
	}// inner class InToNode
	/** This inner class holds the data for a constant outflow to a node. Outflow
	 * in m³/s.*/
	public class OutToNode
	{
		/** Outflow from this node [m³/s].
		 @param value */
		public double value = -1.0;
		/** The node which recieves the outflow.
		 * @param targetNode */
		public Node targetNode = null;
		
		public OutToNode() {};
		public OutToNode( double val, Node node )
		{
			this.value = val;
			this.targetNode = node;
		}
	}// inner class OutToNode
	/** This inner class holds the data for a varring inflow from a node. The function
	 * describing the changing discharge with respect to time is stored in a file.*/
	public class InToNodeFunct
	{
		/** File holding the path of the function file.
		 * @param path */
		public File path = null;
		/** Field holding the node that recieves the inflow*/
		public Node targetNode = null;
		
		public InToNodeFunct() {};
		public InToNodeFunct( File f, Node node)
		{
			this.path = f;
			this.targetNode = node;
		}
	}// inner class InToNodeFunct
	/** This inner class holds the data for a varring outflow from a node. The function
	 * describing the changing discharge with respect to time is stored in a file.*/
	public class OutToNodeFunct
	{
		/** File holding the path of the function file.
		 * @param path */
		public File path = null;
		/** Field holding the node that recieves the outflowing water
		 * @param targetNode*/
		public Node targetNode = null;
		
		public OutToNodeFunct() {};
		public OutToNodeFunct( File f, Node node)
		{
			this.path = f;
			this.targetNode = node;
		}
	}// inner class OutToNodeFunct
	/** This inner class holds the data for a constant outflow to a node. The Outflow
	 * is in m³/s and can only be a certain percentage of the total folow at this node.*/
	public class PercentExtract
	{
		/** Outflow from this node [m³/s] in percent.
		 @param value */
		public double value = -1.0;
		/** Field holding the node that recieves the extracted water (percentage)
		 * @param tragetNode*/
		public Node targetNode = null;
		
		public PercentExtract() {};
		public PercentExtract( double val, Node node )
		{
			this.value = val;
			this.targetNode = node;
		}
	}// inner class PercentExtract
	
	/** This method adds a new set of data to a node ( Type OutToNode ), if the
	 * node has already assigned data it is replaced by the new set of data.
	 * @param val The outflow in m³/s.
	 * @param n The node recieving the outflow.*/
	public void addOutToNode( double val, Node n )
	{
		OutToNode out = new OutToNode( val, n );
		outflowToNode = out;
	}// addOutToNode
	/** This method adds a new set of data to a node ( Type InToNode ), if the
	 * node has already assigned data it is replaced by the new set of data.
	 * @param val The outflow in m³/s.*/
	public void addInToNode( double val )
	{
		InToNode in = new InToNode( val );
		inflowToNode = in;
	}// addInToNode
	/** This method adds a new set of data to a node ( Type PercentExtract ), if
	 * the node has already assigned data it is replaced by the new set of data.
	 * @param val The extrqacted outflow in percent ( value between 0 and 1 ).
	 * @param n The node recieving the outflow.*/
	public void addPercentExtract( double val, Node n )
	{
		PercentExtract p = new PercentExtract( val, n );
		precentExtract = p;
	}// addPercentExtract
	/** This method adds a new set of data to a node ( Type Overflow ), if
	 * the node has already assigned data it is replaced by the new set of data.
	 * @param val The overflow in m³/s.
	 * @param n The node recieving the overflow.*/
	public void addOverflow( double val, Node n )
	{
		OverFlow over = new OverFlow( val, n );
		overFlow = over;
	}// addOverflow
	/** This method adds a new set of data to a node ( Type InToNodeFunct ), if
	 * the node has already assigned data it is replaced by the new set of data.
	 * @param s The string reprecenting the filename or filename and path.
	 * @param n The node recieving the inflow.*/
	public void addInToNodeFunct( String s, Node n )
	{
		File f = new File( s );
		InToNodeFunct inFunct = new InToNodeFunct( f, n );
		inflowToNodeFunct = inFunct;
	}// addInToNodeFunct
	/** This method adds a new set of data to a node ( Type OutToNodeFunct ), if
	 * the node has already assigned data it is replaced by the new set of data.
	 * @param s The string reprecenting the filename or filename and path.
	 * @param n The node recieving the outflow.*/
	public void addOutToNodeFunct( String s, Node n )
	{
		File f = new File( s );
		OutToNodeFunct outFunct = new OutToNodeFunct( f, n );
		outflowToNodeFunct = outFunct;
	}// addOutToNodeFunct
	/** Returns the inflow in m³/s at a node.
	 * @return double Constant in flow to this node*/
	public double getInToNodeVal()	{ return inflowToNode.value; }
	/** Returns the outflow in m³/s to a node.
	 * @return double Constant out flow from this node*/
	public double getOutToNodeVal() { return outflowToNode.value; }
	/** Returns the node which recieves the outflowing water from this node.
	 * @return Node Target node of this node*/
	public Node getOutToNodeTargetNode() { return outflowToNode.targetNode;	}
	/** Returns the overflow in m³/s at a node.
	 * @return double The overflow from this node*/
	public double getOverflowVal() { return overFlow.value; }
	/** Returns the node which recieves the overflowing water from this node.
	 * @return Node Target node of this node*/
	public Node getOverflowNode() { return overFlow.targetNode; }
	/** Returns the file storing the out to node function.
	 * @return File*/
	public File getOutToNodeFunctFile() { return outflowToNodeFunct.path; }
	/** Returns the node which recieves the outflowing water from this node.
	 * @return Node Target node of this node*/
	public Node getOutToNodeFunctNode() { return outflowToNodeFunct.targetNode; }
	/** Returns the file storing the in to node function.
	 * @return File*/
	public File getInToNodeFunctFile() { return inflowToNodeFunct.path; }
	/** Returns this node.
	 * @return Node */
	public Node getInToNodeFunctNode() { return inflowToNodeFunct.targetNode; }
	/** Returns the percentage extracted value at a node ( between 0 and 1 ).
	 * @return double The percentatge extracted from this node*/
	public double getPercentExtractVal() { return precentExtract.value; }
	/** Returns the node which recieves the extracted water from this node.
	 * @return Node Target node of this node*/
	public Node getPercentExtractNode() { return precentExtract.targetNode; }
	/** This function checks if this node has the Type InToNode assigned. If it
	 * assigned this function returns 1 otherwise 0.
	 * @return int 0 or 1*/
	public int isInToNodeExisting()
	{
		if( inflowToNode != null )
			return 1;
		else return 0;
	}// isInToNodeExisting
	/** This function checks if this node has the Type OutToNode assigned. If it
	 * assigned this function returns 1 otherwise 0.
	 * @return int 0 or 1*/
	public int isOutToNodeExisting()
	{
		if( outflowToNode != null )
			return 1;
		else return 0;
	}// isOutToNodeExisting
	/** This function checks if this node has the Type OverFlow assigned. If it
	 * assigned this function returns 1 otherwise 0.
	 * @return int 0 or 1*/
	public int isOverFlowExisting()
	{
		if( overFlow != null )
			return 1;
		else return 0;
	}// isOverFlowExisting
	/** This function checks if this node has the Type InToNodeFunct assigned.
	 * If it assigned this function returns 1 otherwise 0.
	 * @return int 0 or 1*/
	public int isInToNodeFunctExisting()
	{
		if( inflowToNodeFunct != null )
			return 1;
		else return 0;
	}// isInToNodeFunctExisting
	/** This function checks if this node has the Type OutToNodeFunct assigned.
	 * If it assigned this function returns 2 otherwise 0.
	 * @return int 0 or 2*/
	public int isOutToNodeFunctExisting()
	{
		if( outflowToNodeFunct != null )
			return 2;
		else return 0;
	}// isOutToNodeFunctExisting
	/** This function checks if this node has the Type PercentExtract assigned.
	 * If it assigned this function returns 1 otherwise 0.
	 * @return int 0 or 1*/
	public int isPercentExtractExisting()
	{
		if( precentExtract != null )
			return 1;
		else return 0;
	}// isPercentExtractExisting
	/** This method writes the hydrodata of a node to the net file.
	 * @param bw BufferedWriter of the net file.
	 * @exception Throws IOException if the BufferedWriter is corupted.*/
	public void writeHydroDataToFile( BufferedWriter bw ) throws IOException
	{
		
		if( inflowToNode != null )
		{
			bw.newLine();
			String string = Printf.format("%10.3f" , String.valueOf( getInToNodeVal() ) );
			bw.write( string );
		}
		if( outflowToNode != null )
		{
			bw.newLine();
			String s1 = Printf.format("%10.3f" , String.valueOf( getOutToNodeVal() ) );
			String s2 = Printf.format("%8.0d" , String.valueOf( getOutToNodeTargetNode().getNodeNr() ) );
			bw.write( s1 + s2 );
		}
		if( overFlow != null )
		{
			bw.newLine();
			String s1 = Printf.format("%10.3f" , String.valueOf( getOverflowVal() ) );
			String s2 = Printf.format("%8.0d" , String.valueOf( getOverflowNode().getNodeNr() ) );
			bw.write( s1 + s2 );
		}
		if( inflowToNodeFunct != null )
		{
			bw.newLine();
			String s1 = Printf.format("%8.0d" , String.valueOf( getInToNodeFunctNode().getNodeNr() ) );
			bw.write( s1 );
			bw.newLine();
			String s2 = Printf.format("%s" , getInToNodeFunctFile().getPath() );// Ist vom bcena auf 120 Char im netz.cmn definiert
			bw.write( s2 );
		}
		if( outflowToNodeFunct != null )
		{
			bw.newLine();
			String s1 = Printf.format("%8.0d" , String.valueOf( getOutToNodeFunctNode().getNodeNr() ) );
			bw.write( s1 );
			bw.newLine();
			String s2 = Printf.format("%s" , getOutToNodeFunctFile().getPath() );// Ist vom bcena auf 70 Char im netz.cmn deffiniert
			bw.write( s2 );
		}
		if( precentExtract != null )
		{
			bw.newLine();
			String s1 = Printf.format("%10.3f" , String.valueOf( getPercentExtractVal() ) );
			String s2 = Printf.format("%8.0d" , String.valueOf( getPercentExtractNode().getNodeNr() ) );
			bw.write( s1 + s2 );
		}// writeHydroDataToFile
	}
	/** This method returns an array of the hydro flags reprecenting if a type
	 * existst at an node or not.
	 * @return arry */
	
	protected String[] hydroFlagsToArray()
	{
		String s5 = null;
		if( isInToNodeFunctExisting() > 0 )
		{
			s5 = String.valueOf( isInToNodeFunctExisting());
		}
		else
		{
			s5 = String.valueOf( isOutToNodeFunctExisting() );
		}
		String array[] = { null , String.valueOf( isInToNodeExisting()),
				String.valueOf( isOutToNodeExisting() ), String.valueOf( isOverFlowExisting() )
				, s5, String.valueOf( isPercentExtractExisting() ) };
		return array;
	}
}// HydroData

