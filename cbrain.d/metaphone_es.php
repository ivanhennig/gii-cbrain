<?php
// ============================================================================
// Fuction Original Name: spanish_metaphone($string)
// ============================================================================
// Author:      Israel J. Sustaita (isloera@yahoo.com)
// Version:     1.0.1
// Input:       A string
// Output:      Metaphone key string
// Description: This function takes a spanish word and returns its
//              metaphone sound key.
// Comments:
//              It generates spanish  metaphone  keys useful for  spell
//              checkers and other purposes.I decided to alter the
//              metaphone function   because  I needed to check the spelling
//              in spanish words.
//
// History:
//              2005-10-14 - Version 1.0.1
//                 - Removed unnecesary code and fixed some minor bugs
//
//              2005-10-09 - Version 1.0.0
//                 - Initial Release
//
//
//                    **** Acknowledgements ****
//
//      This Function was adapted from a functional callable version of
//      DoubleMetaphone created by Geoff Caplan http://www.advantae.com, who
//      adapted it from the class by  Stephen Woodbridge.
//
//
//      It also uses the "string_at()" and the "is_vowel()" functions  from
//      the same implementation.
//
//               Source:  http://swoodbridge.com/DoubleMetaPhone/
//
// ============================================================================
function metaphone_es($string, $LENGTH = 50) {
	$string_at = function ($STRING, $START, $STRING_LENGTH, $LIST) {
		if (($START < 0) || ($START >= strlen($STRING))) {
			return 0;
		}
		for ($I = 0; $I < count($LIST); $I++) {
			if ($LIST[$I] == substr($STRING, $START, $STRING_LENGTH)) {
				return 1;
			}
		}
		return 0;
	};
	$is_vowel = function ($string, $pos) {
		return preg_match("/A|E|I|O|U/", substr($string, $pos, 1));
	};
   //initialize metaphone key string
   $meta_key   = "";

   //set maximum metaphone key size
   $key_length   = $LENGTH;

   //set current position to the beginning
   $current_pos   =  0;

   //get string  length
   $string_length   = strlen($string);

   //set to  the end of the string
   $end_of_string_pos     = $string_length - 1;
   $original_string = $string. "    ";

   //Let's replace some spanish characters  easily confused
   $original_string = strtr($original_string, 'áéíóúñübz', 'AEIOUNUVS');

   //convert string to uppercase
   $original_string = strtoupper($original_string);


   // main loop
   while (strlen($meta_key) < $key_length)
   {

      //break out of the loop if greater or equal than the length
      if ($current_pos >= $string_length)
      {
         break;
      }

      //get character from the string
      $current_char = substr($original_string, $current_pos, 1);

      //if it is a vowel, and it is at the begining of the string,
      //set it as part of the meta key
      if ($is_vowel($original_string, $current_pos)
                            && ($current_pos == 0))
      {
         $meta_key   .= $current_char;
         $current_pos += 1;
      }
      //Let's check for consonants  that have a single sound
      //or already have been replaced  because they share the same
      //sound like 'B' for 'V' and 'S' for 'Z'
      else if ($string_at($original_string, $current_pos, 1,
              array('D','F','J','K','M','N','P','R','S','T','V')))
      {
         $meta_key   .= $current_char;

         //increment by two if a repeated letter is found
         if (substr($original_string, $current_pos + 1,1) == $current_char)
         {
            $current_pos += 2;
         }

         //else increment only by one
         $current_pos += 1;
      }
      else  //check consonants with similar confusing sounds
      {

         switch ($current_char)
         {

            case 'C':
               //special case 'macho', chato,etc.
               if (substr($original_string, $current_pos + 1,1)== 'H')
               {
                  $current_pos += 2;
               }
               //special case 'acción', 'reacción',etc.
               else if (substr($original_string, $current_pos + 1,1)== 'C')
               {

                  $meta_key   .= 'X';
                  $current_pos += 2;
                  break;
               }
               // special case 'cesar', 'cien', 'cid', 'conciencia'
               else if ($string_at($original_string, $current_pos, 2,
                         array('CE','CI')))
               {
                  $meta_key   .= 'S';
                  $current_pos += 2;
                  break;
               }
               // else
               $meta_key   .= 'K';
               $current_pos += 1;
               break;

            case 'G':
               // special case 'gente', 'ecologia',etc
               if ($string_at($original_string, $current_pos, 2,
                         array('GE','GI')))
               {
                  $meta_key   .= 'J';
                  $current_pos += 2;
                  break;
               }
               // else
               $meta_key   .= 'G';
               $current_pos += 1;
               break;

            //since the letter 'h' is silent in spanish,
            //let's set the meta key to the vowel after the letter 'h'
            case 'H':
               if ($is_vowel($original_string, $current_pos + 1))
               {
                  $meta_key .= $original_string[$current_pos + 1];
                  $current_pos += 2;
                  break;
               }

               // else
               $meta_key   .= 'H';
               $current_pos += 1;
               break;

            case 'Q':
               if (substr($original_string, $current_pos + 1,1) == 'U')
               {
                  $current_pos += 2;
               }
               else
               {
                  $current_pos += 1;
               }

               $meta_key   .= 'K';
               break;

            case 'W':
               $meta_key   .= 'U';
               $current_pos += 2;
               break;

            case 'X':
               //some mexican spanish words like'Xochimilco','xochitl'
               if ($current_pos == 0)
               {
                  $meta_key   .= 'S';
                  $current_pos += 2;
                  break;
               }

               $meta_key   .= 'X';
               $current_pos += 1;
               break;

            default:
               $current_pos += 1;

         } // end of switch


      }//end else


      //Commented code *** for debugging purposes only ***
      /*
      printf("<br>ORIGINAL STRING:    '%s'\n", $original_string);
      printf("<br>CURRENT POSITION:   '%s'\n", $current_pos);
      printf("<br>META KEY STRING:    '%s'\n", $meta_key);
      */

   } // end of while loop


   //trim any blank characters
   $meta_key = trim($meta_key) ;

   //return the final meta key string
   return $meta_key;

}
// ====== End of spanish_metaphone function =======================