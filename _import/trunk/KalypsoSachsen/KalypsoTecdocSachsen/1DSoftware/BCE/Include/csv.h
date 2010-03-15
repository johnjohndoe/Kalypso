/*! Time-stamp: <@(#)assocNN.h   09.03.03 - 13:53:32   Belger>
*********************************************************************
*  @file   : csv.h
*
*  Author  : Belger                              Date: 09.03.03
*
*  Purpose : Declaration of csv
*
*********************************************************************
*/

#pragma warning(disable:4786)
#pragma warning(disable:4503)

#if !defined(_CSV_H_INCLUDED_)
#define _CSV_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <map>
#include <set>
#include <string>

namespace BCE
{
/**
* @class CSV
* 
* Vorlagenklasse für die Moddelierung von Comma-Separated-Values
* 
	*/
	template<typename RowType, typename ColumnType, typename ValueType>
		class CSV
	{
public:
	typedef std::map<ColumnType, ValueType> Row;
	typedef std::map<RowType, Row*> Table;
	typedef std::set<ColumnType> Columns;

	typedef void (*ColumnWriter)( std::ostream, ColumnType );
	
private:
	Table m_table;
	Columns m_columns;
	
	//
	// Konstruktion / Destruktion
	//
public:
	~CSV()
	{
		for( Table::iterator rowIt = m_table.begin(); rowIt != m_table.end(); rowIt++ )
			delete rowIt->second;
	};
	
	
public:
	void PutValue( const RowType& rowIndex, ColumnType columnIndex, ValueType value )
	{
		m_columns.insert( Columns::value_type( columnIndex ) );

		Table::const_iterator rowPos = m_table.find( rowIndex );
		Row* row = rowPos == m_table.end() ? new Row() : rowPos->second;
		row->insert( Row::value_type( columnIndex, value ) );
		
		if( rowPos == m_table.end() )
			m_table.insert( Table::value_type( rowIndex, row ) );
	}
	
	void WriteToStream( std::ostream os, ColumnType rowIndexName, const char separator, ColumnWriter colWriter, ValueType defaultValue )
	{
		colWriter( os, rowIndexName );

		for( Columns::const_iterator colIt = m_columns.begin(); colIt != m_columns.end(); colIt++ )
		{
			os << separator;
			const ColumnType& column = *colIt;
			colWriter( os, column );
		}
		os << std::endl;

		for( Table::const_iterator iter = m_table.begin(); iter != m_table.end(); iter++ )
		{
			RowType rowIndex = iter->first;
			Row* row = iter->second;

			os << rowIndex;
			
			for( Columns::const_iterator colIt = m_columns.begin(); colIt != m_columns.end(); colIt++ )
			{
				os << separator;

				const ColumnType& column = *colIt;
				
				Row::const_iterator colPos = row->find( column );

				ValueType value = colPos == row->end() ? defaultValue : colPos->second;
				
				os << value;
			}

			os << std::endl;
		}
	};
	};
}

#endif // !defined(_CSV_H_INCLUDED_)
