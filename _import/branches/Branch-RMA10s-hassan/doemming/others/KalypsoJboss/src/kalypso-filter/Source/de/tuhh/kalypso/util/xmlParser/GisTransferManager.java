package de.tuhh.kalypso.util.xmlParser;

import java.util.HashMap;
import java.util.Set;
import de.tuhh.kalypso.data.StrandTable;
import de.tuhh.kalypso.data.WcTable;
import de.tuhh.kalypso.data.RbTable;
import de.tuhh.kalypso.data.NodeTable;
import de.tuhh.kalypso.data.riverbasin.Rb;
import de.tuhh.kalypso.data.strand.Strand;
import de.tuhh.kalypso.data.strand.StrandData;
import de.tuhh.kalypso.data.Wc;
import de.tuhh.kalypso.util.LogFile;
import de.tuhh.kalypso.data.node.Node;
import de.tuhh.kalypso.data.node.HydroData;
import java.util.Vector;
import java.io.IOException;
import org.xml.sax.SAXException;
import de.tuhh.kalypso.util.xmlParser.GisTransferObject;
import java.util.Iterator;

public class GisTransferManager implements KalypsoXmlImportListener
{
	private HashMap m_transferObjects = null;
	/** The Constructor of this class starts the SAX-Parser with the given filename.
	 * @param xmlFileName The name of the xml-file to be parsed.
	 */
    public GisTransferManager(String xmlFileName) throws Exception
    {
	XmlImport xmlImport=new XmlImport(xmlFileName,this);
		try
	    {
			xmlImport.start();
	    }
		catch(IOException e) //zB file not found
	    {
			System.out.println("IOException:"+e.getMessage());
			e.printStackTrace();
	    }
		catch(SAXException e) //zB kein XML-Format
	    {
			System.out.println("SAXException:"+e.getMessage());
			e.printStackTrace();
	    }
    }
    public HashMap getTransferObjects( String key )
    {
	if(m_transferObjects.containsKey(key))
	    return (HashMap)m_transferObjects.get( key );
	else
	    return new HashMap();
    }
    public HashMap getAllTransferObjects()
	{
		return (HashMap) m_transferObjects;
	}
	
	public void addTransferObject( String key, GisTransferObject gisTransferObject )
	{
		HashMap newTable  = new HashMap();
		if( getAllTransferObjects() != null )
		{
			HashMap table  = (HashMap) getAllTransferObjects().get( key );
			if( table != null )
				table.put( gisTransferObject.getIdentifier(), gisTransferObject );
			else
			{
				newTable.put( gisTransferObject.getIdentifier(), gisTransferObject );
				getAllTransferObjects().put( key, newTable );
			}
		}
		else
		{
			HashMap listOfTables = new HashMap();
			newTable.put( gisTransferObject.getIdentifier(), gisTransferObject );
			listOfTables.put( key, newTable );
			m_transferObjects = listOfTables;
		}
	}// addTransferObject
	
	/** This method writes all xml objects in a Hashmap. This Hashmap is the basis
	 * to map the xml-objects to this object structure.
	 */
    public void importObject(GisTransferObject gisTransferObject)
    {
		/*System.out.println("-------------------------------------------------------");
		 System.out.println("Object from XML in ");
		 System.out.print("Tabelle: "+gisTransferObject.getTableName() +"\n");*/
		
		String table = gisTransferObject.getTableName();
		
		if( table != null )
		{
			if( table.equals( "nullStrand" )  || table.equals( "channel" ) || table.equals( "rhb" ) || table.equals( "rht" ) )
			{
			    addTransferObject( table, gisTransferObject );
				//System.out.println( "Recieved " + table + "-object" );
			}
			if( gisTransferObject.isRelation() == true )
			{
				addTransferObject( table, gisTransferObject );
				//System.out.println( "Recieved a " + table + "-rel-object" );
			}
			if( table.equals( "rb" ) )
			{
				addTransferObject( table, gisTransferObject );
				//System.out.println( "Recieved " + table + "-object" );
			}
			if( table.equals( "node" ) )
			{
				addTransferObject( table, gisTransferObject );
				//System.out.println( "Recieved " + table + "-object" );
			}
			if( table.equals( "wc" ) )
			{
				addTransferObject( table, gisTransferObject );
				//System.out.println( "Recieved " + table + "-object" );
			}
		}
		
    }
	/** This method invokes the mapping of the gis-transfer-objects.*/
	public void mapGisObjects( StrandTable strands, WcTable wcList, RbTable ribs ) throws Exception
	{
	        mapStrands( strands, wcList, "nullStrand" );
		mapStrands( strands, wcList,"channel" );
		mapStrands( strands, wcList,"rhb" );
		mapStrands( strands, wcList,"rht" );
		mapRelNodes( strands );
		mapRb( ribs, strands );
		//strands.dump();
		
	}// mapGisObjects
	public void mapRb( RbTable ribs, StrandTable strands )
	{
		HashMap rel = new HashMap();
		HashMap rbs = getTransferObjects( "rb" );
		if( rbs != null )
		{
			Iterator it = rbs.values().iterator();
			while( it.hasNext() )
			{
				GisTransferObject gto = (GisTransferObject) it.next();
				GisTransferObject gisRel = getObjectsFromRel( "rb2strand", gto.getIdentifier(), "s" );
				GisTransferObject gisS = getObjectsFromMap( gisRel.getRelationDestTable(), gisRel.getRelationDestIdentifier() );
				Strand s = strands.getStrand( Integer.parseInt( gisS.getSimpleProperty( "m_strandNr" )));
				
				Rb rb = new Rb( Integer.parseInt( gto.getSimpleProperty( "m_rbNumber" ) ));
				rb.setFlagHydrotop( gto.getSimpleProperty( "m_flagHydrotop" ));
				rb.setStrandRb( s );
				if ( !gto.getSimpleProperty("m_comment").equals("null"))
					rb.setComment( gto.getSimpleProperty( "m_comment" ));
				rb.setSealedAreaRb( Double.parseDouble( gto.getSimpleProperty( "m_sealedAreaRb" )));
				rb.setNatAreaRb( Double.parseDouble( gto.getSimpleProperty( "m_natAreaRb" )));
				if( gto.getSimpleProperty( "m_corrRain") != null )
					rb.setCorrRain( Double.parseDouble( gto.getSimpleProperty( "m_corrRain" )));
				rb.setCorrMaxInter( Double.parseDouble( gto.getSimpleProperty( "m_corrMaxInter" )));
				rb.setCorrInitInter( Double.parseDouble( gto.getSimpleProperty( "m_corrInitInter" )));
				rb.setCorrUsage( Double.parseDouble( gto.getSimpleProperty( "m_corrUsage" )));
				rb.setKey( gto.getSimpleProperty( "m_key" ));
				if( !gto.getSimpleProperty( "m_fileClimate" ).equals("null" ))
					rb.setFileClimate( gto.getSimpleProperty( "m_fileClimate" ));
				if( !gto.getSimpleProperty( "m_fileLongTermSim" ).equals("null" ))
					rb.setFileLongTerm( gto.getSimpleProperty( "m_fileLongTermSim" ));
				if( !gto.getSimpleProperty( "m_fileShortTermSim" ).equals("null" ))
					rb.setFileShortTerm( gto.getSimpleProperty( "m_fileShortTermSim" ));
				if( !gto.getSimpleProperty( "m_fileTimeAreaFunct" ).equals("null" ))
					rb.setFileTimeAreaFunct( gto.getSimpleProperty( "m_fileTimeAreaFunct" ));
				if( !gto.getSimpleProperty( "m_fileHydrotop" ).equals("null" ))
					rb.setFileHydrotop( gto.getSimpleProperty( "m_fileHydrotop" ));
				rb.setSnowWaterCont( Double.parseDouble( gto.getSimpleProperty( "m_snowWaterCont" )));
				rb.setSnowMaxWaterCont( Double.parseDouble( gto.getSimpleProperty( "m_snowMaxWaterCont" )));
				rb.setSnowMeltingRateTemp( Double.parseDouble( gto.getSimpleProperty( "m_snowMeltingRateTemp" )));
				rb.setSnowMeltingRateRad( Double.parseDouble( gto.getSimpleProperty( "m_snowMeltingRateRad" )));
				rb.setInitSnowHeight( Double.parseDouble( gto.getSimpleProperty( "m_initSnowHeight" )));
				rb.setCorrTemp( Double.parseDouble( gto.getSimpleProperty( "m_corrTemp" )));
				rb.setCorrEvap( Double.parseDouble( gto.getSimpleProperty( "m_corrEvap" )));
				
				// Maps the VectorSet
				rb.mapRbCorrFact( gto );
				
				rb.setCorrUsage( Double.parseDouble( gto.getSimpleProperty( "m_corrUsage" )));
				rb.setInitInterception( Double.parseDouble( gto.getSimpleProperty( "m_initInterception" )));
				rb.setInitAquif( Double.parseDouble( gto.getSimpleProperty( "m_initAquif" )));
				rb.setRetSealedArea( Double.parseDouble( gto.getSimpleProperty( "m_retSealedArea" )));
				rb.setRetOverlandFlow( Double.parseDouble( gto.getSimpleProperty( "m_retOverlandFlow" )));
				rb.setRetInterflow( Double.parseDouble( gto.getSimpleProperty( "m_retInterflow" )));
				rb.setRetAquif( Double.parseDouble( gto.getSimpleProperty( "m_retAquif" )));
				rb.setRetAquifNeighbour( Double.parseDouble( gto.getSimpleProperty( "m_retAquifNeighbour" )));
				rb.setRetOutflowAquifDeep( Double.parseDouble( gto.getSimpleProperty( "m_retOutflowAquifDeep" )));
				rb.setAquifMinHeightChannel( Double.parseDouble( gto.getSimpleProperty( "m_aquifMinHeightChannel" )));
				rb.setAquifMaxHeightChannel( Double.parseDouble( gto.getSimpleProperty( "m_aquifMaxHeightChannel" )));
				rb.setAquifSplitAquiferDeep( Double.parseDouble( gto.getSimpleProperty( "m_aquifSplitAquiferDeep" )));
				rb.setAquifPorosity( Double.parseDouble( gto.getSimpleProperty( "m_aquifPorosity" )));
				rb.setAquifExtract( Double.parseDouble( gto.getSimpleProperty( "m_aquifExtract" )));
				
				//DEEP AQUIFER STRANDS NOT FULLY IMPLEMENTED (CK: 15.1.2003)
				
				GisTransferObject gisDeepRel = getObjectsFromRel( "rb2deepAquif", gto.getIdentifier(), "d" );
				if( gisDeepRel != null )
				{
					rb.setAquifInflowDeepAquifVal( Double.parseDouble( gisDeepRel.getSimpleProperty( "m_aquifInflowDeepAquifVal" )));
					//is not clear if it has to be a strand or a node  !! very important if deep aquifernet is used
					Strand sDeep = strands.getStrand(
						Integer.parseInt( getObjectsFromMap( gisRel.getRelationDestTable(), gisRel.getRelationDestIdentifier()).getSimpleProperty( "m_strandNr")));
					rb.setAquifTargetStrandDeepAquif( sDeep );
				}
				ribs.add( rb );
			}// while
		}// if
		mapRbRel( ribs, strands );
	}
	public void mapRbRel( RbTable ribs, StrandTable strands )
	{
		HashMap map = getTransferObjects( "rb" );
		HashMap rel = null;
		Iterator it = map.values().iterator();
		while( it.hasNext() )
		{
			
			GisTransferObject gto = (GisTransferObject) it.next();
			Rb srcRb = ribs.getRb( Integer.parseInt( gto.getSimpleProperty( "m_rbNumber" )));
			rel = getRbToRbRel( gto );
			if( rel != null && rel.size() != 0 )
			{
				Iterator itrel = rel.values().iterator();
				while( itrel.hasNext() )
				{
					GisTransferObject o = (GisTransferObject) itrel.next();
					Rb targetRb = ribs.getRb( Integer.parseInt( getObjectsFromMap( "rb", o.getRelationDestIdentifier()).getSimpleProperty( "m_rbNumber" )));
					double value = Double.parseDouble( o.getSimpleProperty( "value"  ));
					srcRb.createRbOutConnected( targetRb, value );
				}// while
			}// if
			// DEEP AQUIFER NOT FULLY IMPLEMENTED (CK: 15.1.2003)
			
			/*rel = getRbToDeepAquif( gto );
			if( rel != null )
			{
				Set set = rel.keySet();
				Iterator itSet = set.iterator();
				String s = (String) itSet.next();
				GisTransferObject gis = getStrand( s );
				Strand strand = strands.getStrand( Integer.parseInt( gis.getSimpleProperty( "m_strandNr" )));
				srcRb.setAquifTargetStrandDeepAquif( strand );
				srcRb.setAquifInflowDeepAquifVal( Double.parseDouble( (String) rel.get( s )));
			 }// if*/
		}// while
	}// mapRbToRb
	public void mapStrands( StrandTable strands, WcTable wcList, String strandType ) throws Exception
	{
		
	    HashMap strand = getTransferObjects( strandType );
		
		if( strand != null )
		{
			Iterator it = strand.values().iterator();
			while( it.hasNext() )
			{
				
				GisTransferObject gto = (GisTransferObject) it.next();
				
				int number = Integer.parseInt( gto.getSimpleProperty( "m_strandNr") );
				
				HashMap strandRel = getRelationsForObject( gto ); //getStrandRel( gto, strandType );
				//dump(strandRel);
				// relation to nodeIn
				
				HashMap map = (HashMap) strandRel.get( "node2strand" );
				GisTransferObject gisIn = (GisTransferObject) map.get( "0" );
				
				int nodeNr = Integer.parseInt( getObjectsFromMap( "node", gisIn.getRelationSrcIdentifier()).getSimpleProperty("m_nodeNr"));
				Node inNode = strands.getNodeFromStrand( nodeNr );
				// checking if the node already exists
				if ( inNode != null )
					inNode = inNode;
				else // if not generate it
				{
					Node in = new Node( nodeNr );
					inNode = in;
				}
				String nodeProperty =  getObjectsFromMap( "node", gisIn.getRelationSrcIdentifier()).getSimpleProperty("m_inToNodeConst");
				if( nodeProperty != null )
					mapNodeConstIn( nodeProperty, inNode );
				
				// relation to nodeOut
				map = (HashMap) strandRel.get( "strand2node" );
				GisTransferObject gisOut = (GisTransferObject) map.get( "0" );
				//GisTransferObject gisOut = (GisTransferObject) strandRel.get( "strand2node" );
				nodeNr = Integer.parseInt( getObjectsFromMap( "node", gisOut.getRelationDestIdentifier()).getSimpleProperty("m_nodeNr"));
				Node outNode = strands.getNodeFromStrand( nodeNr );
				// checking if the node already exists
				if ( outNode != null )
					outNode = outNode;
				else // if not generate it
				{
					Node out = new Node( nodeNr );
					outNode = out;
				}
				nodeProperty =  getObjectsFromMap( "node", gisOut.getRelationDestIdentifier()).getSimpleProperty("m_inToNodeConst");
				if( nodeProperty != null )
					mapNodeConstIn( nodeProperty, outNode );
				// relation to wcIndex
				
				map = (HashMap) strandRel.get( "wc2objects" );
				GisTransferObject gisWc = (GisTransferObject) map.get( "0" );
				//GisTransferObject gisWc = (GisTransferObject) strandRel.get( "wc2objects" );
				String name = getObjectsFromMap( "wc", gisWc.getRelationSrcIdentifier() ).getSimpleProperty("m_wcName");
				String index = getObjectsFromMap( "wc", gisWc.getRelationSrcIdentifier() ).getSimpleProperty( "m_wcIndex" );;
				Wc wc = wcList.addNew( name, index );
				
				Strand s = new Strand( number, inNode, outNode, wc );
				
				strands.add( s );
				
				if( strandType.equals( "nullStrand" ))
				{
					s.setType( 0 );
				}
				if( strandType.equals( "channel" ))
				{
					StrandData sd = s.setType( 1 );
					sd.mapStrandData( gto );
				}
				if( strandType.equals( "rhb" ))
				{
					StrandData sd = s.setType( 2 );
					sd.mapStrandData( gto );
				}
				if( strandType.equals( "rht" ))
				{
					StrandData sd = s.setType( 3 );
					sd.mapStrandData( gto );
				}
			}// while
		}
	}// mapStrands
	
	public GisTransferObject getObjectsFromRel( String key, String id, String type )
	{
		
		HashMap subset = getTransferObjects( key );
		if( subset != null )
		{
			Iterator it = subset.values().iterator();
			while( it.hasNext() )
			{
				GisTransferObject gto = (GisTransferObject) it.next();
				if( type.equals( "d" ) && gto.getRelationDestIdentifier().equals( id ) )
					return gto;
				if( type.equals( "s" ) && gto.getRelationSrcIdentifier().equals( id ) )
					return gto;
			}
		}
		return null;
	}
	
	
	public GisTransferObject getObjectsFromMap( String key, String id )
	{
		GisTransferObject gto = (GisTransferObject) getTransferObjects( key ).get( id );
		return gto;
	}
	
	public void mapNodeConstIn( String value, Node node )
	{
		
		HydroData hd = node.getHydroData();
		if( hd != null && ( hd.isInToNodeExisting() != 0) )
			hd.addInToNode( Double.parseDouble( value ));
		else
			node.createHydroData().addInToNode( Double.parseDouble( value ) );
		
	}// mapNode
	public void mapRelNodes( StrandTable strands )
	{
		HashMap nodes = getTransferObjects( "node" );
		Iterator it = nodes.values().iterator();
		while( it.hasNext() )
		{
			GisTransferObject node = (GisTransferObject) it.next();
			//HashMap rel_alt = getNodeRel( node );
			HashMap rel = getRelationsForObject( node );
			if( rel != null )
			{
				String idSource = null;
				String idTarget = null;
				GisTransferObject g = null;
				HashMap mapOfmap = (HashMap) rel.get( "node2nodeOverflow" );
				//GisTransferObject g = (GisTransferObject) rel.get( "node2nodeOverflow" );
				if( mapOfmap != null )
				{
					for( int i = 0; i < mapOfmap.size(); i++)
					{
						g = (GisTransferObject) mapOfmap.get( String.valueOf( i ));
						double value = Double.parseDouble( g.getSimpleProperty( "value" ));
						idSource = g.getRelationSrcIdentifier();
						idTarget = g.getRelationDestIdentifier();
						GisTransferObject source = (GisTransferObject) nodes.get( idSource );
						GisTransferObject traget = (GisTransferObject) nodes.get( idTarget );
						Node ns = strands.getNodeFromStrand( Integer.parseInt( source.getSimpleProperty("m_nodeNr") ) );
						Node nt = strands.getNodeFromStrand( Integer.parseInt( traget.getSimpleProperty("m_nodeNr") ) );
						if( ns.getHydroData()!= null )
							ns.getHydroData().addOverflow( value, nt );
						else
							ns.createHydroData().addOverflow( value, nt );
					}
				}
				
				mapOfmap = (HashMap) rel.get( "node2nodeFunctOut" );
				//g = (GisTransferObject) rel.get( "node2nodeFunctOut" );
				if( mapOfmap != null )
				{
					for( int i = 0; i < mapOfmap.size(); i++)
					{
						g = (GisTransferObject) mapOfmap.get( String.valueOf( i ));
						String path = g.getSimpleProperty( "path" );
						idSource = g.getRelationSrcIdentifier();
						idTarget = g.getRelationDestIdentifier();
						GisTransferObject source = (GisTransferObject) nodes.get( idSource );
						GisTransferObject traget = (GisTransferObject) nodes.get( idTarget );
						Node ns = strands.getNodeFromStrand( Integer.parseInt( source.getSimpleProperty("m_nodeNr") ) );
						Node nt = strands.getNodeFromStrand( Integer.parseInt( traget.getSimpleProperty("m_nodeNr") ) );
						if( ns.getHydroData()!= null )
							ns.getHydroData().addOutToNodeFunct( path, nt );
						else
							ns.createHydroData().addOutToNodeFunct( path, nt );
					}
				}
				mapOfmap = (HashMap) rel.get( "node2nodeFunctIn" );
				//g = (GisTransferObject) rel.get( "node2nodeFunctIn" );
				if( mapOfmap != null )
				{
					for( int i = 0; i < mapOfmap.size(); i++)
					{
						g = (GisTransferObject) mapOfmap.get( String.valueOf( i ));
						String path = g.getSimpleProperty( "path" );
						idSource = g.getRelationSrcIdentifier();
						idTarget = g.getRelationDestIdentifier();
						GisTransferObject source = (GisTransferObject) nodes.get( idSource );
						GisTransferObject traget = (GisTransferObject) nodes.get( idTarget );
						Node ns = strands.getNodeFromStrand( Integer.parseInt( source.getSimpleProperty("m_nodeNr") ) );
						Node nt = strands.getNodeFromStrand( Integer.parseInt( traget.getSimpleProperty("m_nodeNr") ) );
						if( ns.getHydroData()!= null )
							ns.getHydroData().addInToNodeFunct( path, nt );
						else
							ns.createHydroData().addInToNodeFunct( path, nt );
					}
				}
				mapOfmap = (HashMap) rel.get( "node2nodeConstOut" );
				//g = (GisTransferObject) rel.get( "node2nodeConstOut" );
				if( mapOfmap != null )
				{
					for( int i = 0; i < mapOfmap.size(); i++)
					{
						g = (GisTransferObject) mapOfmap.get( String.valueOf( i ));
						double value = Double.parseDouble( g.getSimpleProperty( "value" ));
						idSource = g.getRelationSrcIdentifier();
						idTarget = g.getRelationDestIdentifier();
						GisTransferObject source = (GisTransferObject) nodes.get( idSource );
						GisTransferObject traget = (GisTransferObject) nodes.get( idTarget );
						Node ns = strands.getNodeFromStrand( Integer.parseInt( source.getSimpleProperty("m_nodeNr") ) );
						Node nt = strands.getNodeFromStrand( Integer.parseInt( traget.getSimpleProperty("m_nodeNr") ) );
						if( ns.getHydroData()!= null )
							ns.getHydroData().addOutToNode( value, nt );
						else
							ns.createHydroData().addOutToNode( value, nt );
					}
				}
				mapOfmap = (HashMap) rel.get( "node2nodeConstOut" );
				//g = (GisTransferObject) rel.get( "node2nodePercent" );
				if( mapOfmap != null )
				{
					for( int i = 0; i < mapOfmap.size(); i++)
					{
						g = (GisTransferObject) mapOfmap.get( String.valueOf( i ));
						double value = Double.parseDouble( g.getSimpleProperty( "value" ));
						idSource = g.getRelationSrcIdentifier();
						idTarget = g.getRelationDestIdentifier();
						GisTransferObject source = (GisTransferObject) nodes.get( idSource );
						GisTransferObject traget = (GisTransferObject) nodes.get( idTarget );
						Node ns = strands.getNodeFromStrand( Integer.parseInt( source.getSimpleProperty("m_nodeNr") ) );
						Node nt = strands.getNodeFromStrand( Integer.parseInt( traget.getSimpleProperty("m_nodeNr") ) );
						if( ns.getHydroData()!= null )
							ns.getHydroData().addPercentExtract( value, nt );
						else
							ns.createHydroData().addPercentExtract( value, nt );
					}
				}
			}// if rel
		}// while
	}// mapRelNodes
	public HashMap getRbToRbRel( GisTransferObject gto )
	{
		String objectId = gto.getIdentifier();
		HashMap map = new HashMap();
		HashMap rel = getTransferObjects( "rb2rb" );
		if( rel != null )
		    {
			Iterator it = rel.values().iterator();
			while( it.hasNext() )
			    {
				GisTransferObject o = (GisTransferObject) it.next();
				/*	if( ( gto.getIdentifier().equals( o.getRelationSrcIdentifier() )
					&& gto.getTableName().equals(o.getRelationSrcTable()))
					|| ( gto.getIdentifier().equals( o.getRelationDestIdentifier() )
					&& gto.getTableName().equals( o.getRelationDestTable())))
				*/
				if( o.getRelationSrcIdentifier().equals( objectId ) )//&& ( o.hashCode() != gto.hashCode() ))
				    {
					GisTransferObject g = getObjectsFromRel( "rb2rb", o.getRelationDestIdentifier(), "d" );
					map.put( g.getIdentifier(), o );
				    }
			    }//while
			return map;
		    }
		return null;
	}
    
    public HashMap getRbToDeepAquif( GisTransferObject gto )
    {
		String objectId = gto.getIdentifier();
		HashMap map = new HashMap();
		HashMap rel = getTransferObjects( "rb2deepAquif" );
		if( rel != null )
		{
			Iterator it = rel.values().iterator();
			while( it.hasNext() )
			{
				GisTransferObject o = (GisTransferObject) it.next();
				if( o.getRelationSrcIdentifier().equals( objectId ) && ( o.hashCode() != gto.hashCode() ))
				{
					/*GisTransferObject g = getStrand( o.getRelationDestIdentifier() );
					 map.put( g.getIdentifier(), o.getSimpleProperty( "value" ));*/
				}
			}//while
			return map;
		}
		return null;
	}
	/*	public GisTransferObject getStrand( String key, String id )
	 {
	 GisTransferObject result = getObjectsFromMap( key, id );
	 if( result != null )
	 return result;
	 return null;
	 }*/
	public void nrOfObjectsToLogFile()
	{
		LogFile.log ( "List of mapped Objects:" );
		Set keySet = getAllTransferObjects().keySet();
		Iterator iterator = keySet.iterator();
		while( iterator.hasNext() )
		{
			String key = (String) iterator.next();
		}
		LogFile.log( "---------------------------------------------------------------------------" );
		if( getTransferObjects("nullStrand") != null )
			LogFile.log( getTransferObjects("nullStrand").size() + " null strands have been mapped." );
		if( getTransferObjects("channel") != null )
			LogFile.log( getTransferObjects("channel").size() + " channel strands have been mapped." );
		if( getTransferObjects("rhb") != null )
			LogFile.log( getTransferObjects("rhb").size() + " reservoir strands have been mapped." );
		if( getTransferObjects("rht") != null )
			LogFile.log( getTransferObjects("rht").size() + " dam strands have been mapped." );
		if( getTransferObjects("rb") != null )
			LogFile.log( getTransferObjects("rb").size() + " river basins have been mapped." );
		if( getTransferObjects("node") != null )
			LogFile.log( getTransferObjects("node").size() + " nodes have been mapped." );
		if( getTransferObjects("wc") != null )
			LogFile.log( getTransferObjects("wc").size() + " watercours indecies have been mapped." );
		LogFile.log( "---------------------------------------------------------------------------" );
		
	}
	public HashMap getRelationsForObject( GisTransferObject gto )
	{
		HashMap rel = new HashMap();
		Iterator itkey = getAllTransferObjects().keySet().iterator();
		while( itkey.hasNext() )
		{
			String key1 = (String) itkey.next();
			HashMap group = getTransferObjects( key1 );
			Iterator itgroup = group.values().iterator();
			while( itgroup.hasNext() )
			{
				GisTransferObject o = (GisTransferObject) itgroup.next();
				int i = 0;
				if( o.isRelation() == true
				   && ( gto.getIdentifier().equals( o.getRelationSrcIdentifier() )
						   && gto.getTableName().equals(o.getRelationSrcTable()))
				   || ( gto.getIdentifier().equals( o.getRelationDestIdentifier() )
						   && gto.getTableName().equals( o.getRelationDestTable())))
				{
					HashMap map = new HashMap();
					String keyNr = String.valueOf( i );
					map.put( keyNr, o );
					rel.put( o.getTableName(), map );
					i = i + 1;
					if( i > 1 )
						System.out.println( i );
				}
			}
		}
		return rel;
	}
	public void dump( HashMap map )
	{
		Iterator it = map.keySet().iterator();
		while( it.hasNext() )
		{
			String key = (String) it.next();
			HashMap mapOfmap = (HashMap) map.get( key );
			Iterator itMapOfMap = mapOfmap.values().iterator();
			while( itMapOfMap.hasNext() )
			{
				GisTransferObject gto = (GisTransferObject)	itMapOfMap.next();
				System.out.println( key );
				gto.info();
			}//while
		}//while
	}//dump
}

